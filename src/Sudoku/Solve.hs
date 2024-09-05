{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-} -- For declaring Barbie types
{-# LANGUAGE RankNTypes #-} -- For smapGrid
module Sudoku.Solve (Solvable, propagator, PropagatorCmd(..), PropagatorResult(..)) where

import Clash.Prelude hiding (mapAccumR)
import RetroClash.Utils hiding (changed)
import RetroClash.Barbies

import Sudoku.Grid
import Sudoku.Matrix

import Data.Maybe
import Barbies.TH
import Data.Monoid.Action

shiftInGridAtN :: forall n m a. (KnownNat n, KnownNat m) => Grid n m a -> a -> (a, Grid n m a)
shiftInGridAtN grid x = (x', unflattenGrid grid')
  where
    (grid', x' :> Nil) = shiftInAtN (flattenGrid grid) (x :> Nil)

neighboursMasks
    :: forall n m. (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 2 <= n * m)
    => Grid n m (Mask n m)
    -> (Bool, Grid n m (Mask n m))
neighboursMasks masks = (failed, rows .<>. columns .<>. boxes)
  where
    (.<>.) = liftA2 (<>)

    row_masks :: Vec (n * m) (Mask n m)
    (row_failed, row_masks) = unzip $ rowmap combineRegion masks
    (col_failed, col_masks) = unzip $ colmap combineRegion masks
    (box_failed, box_masks) = unzip $ toRowMajorOrder $ boxmap combineRegion masks

    rows = gridFromRows . fmap repeat $ row_masks
    columns = gridFromRows . repeat $ col_masks
    boxes = fromBoxes . fmap repeat $ fromRowMajorOrder box_masks

    failed = any_row_failed || any_col_failed || any_box_failed

    any_row_failed = bitToBool . reduceOr $ row_failed
    any_col_failed = bitToBool . reduceOr $ col_failed
    any_box_failed = bitToBool . reduceOr $ box_failed

combineRegion :: forall n m. (KnownNat n, KnownNat m, 1 <= n * m) => Vec (n * m) (Mask n m) -> (Bool, Mask n m)
combineRegion masks = (failed, fold @(n * m - 1) (<>) masks)
  where
    mat = map (map toRegionBit) . transpose . map (bv2v . maskBits) $ masks
    failed = bitToBool . reduceOr $ map (== Failed) $ fold @(n * m - 1) (<>) <$> mat

data RegionBit =  Lo | Failed | Hi
    deriving (Show, ShowX, Generic, NFDataX, Eq, Ord, BitPack)

instance Semigroup RegionBit where
    Hi <> a = a
    a <> Hi = a
    _ <> _ = Failed

toRegionBit :: Bit -> RegionBit
toRegionBit 0 = Lo
toRegionBit 1 = Hi

declareBareB [d|
  data CellUnit n m = CellUnit
    { cell :: Cell n m
    , mask :: Mask n m
    , is_unique :: Bool
    , changed :: Bool
    , cont :: Cell n m
    , keep_guessing :: Bool
    } |]

type Solvable n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, 1 <= n * m * m * n)

data PropagatorCmd
    = Propagate
    | CommitGuess

data PropagatorResult
    = Solved
    | Failure
    | Guess
    | Progress
    deriving (Generic, NFDataX, Eq, Show)

smapGrid :: forall n m a b. (KnownNat n, KnownNat m) => (forall i j k l. (SNat i, SNat j, SNat k, SNat l) -> a -> b) -> Grid n m a -> Grid n m b
smapGrid f = Grid . smapMatrix (\(i, j) -> smapMatrix (\(k, l) -> f (i, j, k, l))) . getGrid

smapMatrix :: forall n m a b. (KnownNat n, KnownNat m) => (forall i j. (SNat i, SNat j) -> a -> b) -> Matrix n m a -> Matrix n m b
smapMatrix f = FromRows . smap (\i -> smap (\j -> f (i, j))) . matrixRows

propagator
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom (Maybe PropagatorCmd)
    -> Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Cell n m)
       , Signal dom PropagatorResult
       , Grid n m (Signal dom (Cell n m))
       )
propagator cmd shift_in pop = (head @(n * m * m * n - 1) (flattenGrid cells), result, conts)
  where
    pops :: Grid n m (Signal dom (Maybe (Cell n m)))
    pops = unbundle . fmap sequenceA $ pop

    (overlapping_uniques, unbundle -> neighbours_masks) = unbundle . fmap neighboursMasks . bundle $ masks

    units :: Grid n m (Signals dom (CellUnit n m))
    units = smapGrid (\i (shift_in, try_guess, pop, neighbour_masks) -> unit i shift_in try_guess pop neighbour_masks) $ pure (,,,) <*> shift_ins <*> prev_guesses <*> pops <*> neighbours_masks

    (_shift_out, shift_ins) = shiftInGridAtN (enable (isJust <$> shift_in) . cell <$> units) shift_in
    (_guessing_failed, prev_guesses) = shiftInGridAtN (keep_guessing <$> units) (pure True)

    masks = mask <$> units
    cells = cell <$> units
    conts = cont <$> units

    fresh = isJust <$> shift_in .||. isJust <$> pop
    all_unique = bitToBool . reduceAnd <$> bundle (is_unique <$> units)
    any_changed = bitToBool . reduceOr <$> bundle (changed <$> units)
    any_failed = bitToBool . reduceOr <$> bundle ((.== conflicted) <$> cells)
    can_guess = not <$> all_unique

    result =
        mux fresh (pure Progress) $
        mux (overlapping_uniques .||. any_failed) (pure Failure) $
        mux all_unique (pure Solved) $
        mux any_changed (pure Progress) $
        mux can_guess (pure Guess) $
        pure Failure

    (enable_propagate, enable_guess) = unbundle do
        cmd <- cmd
        pure $ case cmd of
            Just Propagate -> (True, False)
            Just CommitGuess -> (False, True)
            _ -> (False, False)

    unit
        :: (SNat i, SNat j, SNat k, SNat l)
        -> Signal dom (Maybe (Cell n m))
        -> Signal dom Bool
        -> Signal dom (Maybe (Cell n m))
        -> Signal dom (Mask n m)
        -> Signals dom (CellUnit n m)
    unit (SNat, SNat, SNat, SNat) shift_in try_guess pop neighbours_mask = CellUnit{..}
      where
        shift_out = enable (isJust <$> shift_in) cell

        cell = register conflicted cell'
        mask = mux is_unique (cellMask <$> cell) (pure mempty)

        cell' = do
            current <- cell
            shift_in <- shift_in
            pop <- pop
            can_propagate <- enable_propagate .&&. not <$> is_unique
            use_guess <- enable_guess .&&. guess_this
            guess <- first_guess
            mask <- neighbours_mask
            pure if
                | Just load <- shift_in -> load
                | Just load <- pop      -> load
                | use_guess             -> guess
                | can_propagate         -> act mask current
                | otherwise             -> current

        changed = cell' ./=. cell

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_unique = next_guess .== conflicted

        can_guess = not <$> is_unique
        guess_this = try_guess .&&. can_guess
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. is_unique
