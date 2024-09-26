{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve (Solvable, propagator, PropagatorCmd(..), PropagatorResult(..)) where

import Clash.Prelude hiding (mapAccumR)
import RetroClash.Utils hiding (changed)

import Sudoku.Matrix
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid.Action

shiftInGridAtN :: forall n m a. (KnownNat n, KnownNat m) => Grid n m a -> a -> (a, Grid n m a)
shiftInGridAtN grid x = (x', unflattenGrid grid')
  where
    (grid', x' :> Nil) = shiftInAtN (flattenGrid grid) (x :> Nil)

type Solvable n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, 1 <= n * m * m * n)

neighboursMasks
    :: forall n m. (Solvable n m)
    => Grid n m (Mask n m)
    -> (Bool, Grid n m (Mask n m))
neighboursMasks masks = (failed, rows .<>. columns .<>. boxes)
  where
    (.<>.) = liftA2 (<>)

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

combineRegion :: forall n m. (Solvable n m) => Vec (n * m) (Mask n m) -> (Bool, Mask n m)
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

data CellUnit dom n m = CellUnit
    { cell :: Signal dom (Cell n m)
    , mask :: Signal dom (Mask n m)
    , is_unique :: Signal dom Bool
    , is_conflicted :: Signal dom Bool
    , changed :: Signal dom Bool
    , cont :: Signal dom (Cell n m)
    , keep_guessing :: Signal dom Bool
    }

data PropagatorCmd
    = Propagate
    | CommitGuess
    deriving (Generic, NFDataX, Eq, Show)

data PropagatorResult
    = Progress
    | Solved
    | Failure
    | Stuck
    deriving (Generic, NFDataX, Eq, Show)

propagator
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom (Maybe PropagatorCmd)
    -> Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Cell n m)
       , Signal dom PropagatorResult
       , Signal dom (Sudoku n m)
       )
propagator cmd shift_in pop = (headGrid (cell <$> units), result, bundle $ cont <$> units)
  where
    pops :: Grid n m (Signal dom (Maybe (Cell n m)))
    pops = unbundle . fmap sequenceA $ pop

    (overlapping_uniques, unbundle -> neighbours_masks) = unbundle . fmap neighboursMasks . bundle $ mask <$> units

    units :: Grid n m (CellUnit dom n m)
    units = pure unit <*> shift_ins <*> prev_guesses <*> pops <*> neighbours_masks

    (_, shift_ins) = shiftInGridAtN (enable (isJust <$> shift_in) . cell <$> units) shift_in
    (_, prev_guesses) = shiftInGridAtN (keep_guessing <$> units) (pure True)

    all_unique = bitToBool . reduceAnd <$> bundle (is_unique <$> units)
    any_changed = bitToBool . reduceOr <$> bundle (changed <$> units)
    any_failed = bitToBool . reduceOr <$> bundle (is_conflicted <$> units)

    result =
        mux overlapping_uniques  (pure Failure) $
        mux any_failed           (pure Failure) $
        mux all_unique           (pure Solved) $
        mux any_changed          (pure Progress) $
        pure Stuck

    unit
        :: Signal dom (Maybe (Cell n m))
        -> Signal dom Bool
        -> Signal dom (Maybe (Cell n m))
        -> Signal dom (Mask n m)
        -> CellUnit dom n m
    unit shift_in try_guess pop neighbours_mask = CellUnit{..}
      where
        shift_out = enable (isJust <$> shift_in) cell

        cell = register conflicted cell'
        mask = mux is_unique (cellMask <$> cell) (pure mempty)

        cell' = do
            current <- cell
            shift_in <- shift_in
            pop <- pop
            cmd <- cmd
            guess_this <- guess_this
            is_unique <- is_unique
            first_guess <- first_guess
            neighbours_mask <- neighbours_mask
            pure if
                | Just load <- shift_in                -> load
                | Just load <- pop                     -> load
                | Just Propagate <- cmd, not is_unique -> act neighbours_mask current
                | Just CommitGuess <- cmd, guess_this  -> first_guess
                | otherwise                            -> current

        -- cell' =
        --     shift_in .<|>.
        --     pop .<|>.
        --     enable (cmd .== Just Propagate .&&. not <$> is_unique) (act <$> neighbours_mask <*> cell) .<|>.
        --     enable (cmd .== Just CommitGuess .&&. guess_this) first_guess .<|.
        --     cell

        changed = cell' ./=. cell

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_unique = next_guess .== conflicted
        is_conflicted = cell .== conflicted

        can_guess = not <$> is_unique
        guess_this = try_guess .&&. can_guess
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. is_unique
