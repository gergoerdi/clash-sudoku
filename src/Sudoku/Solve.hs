{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-} -- For declaring Barbie types
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
    :: forall n m dom. (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 2 <= n * m)
    => (HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Mask n m))
    -> (Signal dom Bool, Grid n m (Signal dom (Mask n m)))
neighboursMasks masks = (failed, combine <$> rows <*> columns <*> boxes)
  where
    (.<>.) = liftA2 (<>)

    row_masks :: Vec (n * m) (Signal dom (Mask n m))
    (row_failed, row_masks) = unzip $ rowmap (unbundle . fmap combineRegion . bundle) masks
    (col_failed, col_masks) = unzip $ colmap (unbundle . fmap combineRegion . bundle) masks
    (box_failed, box_masks) = unzip $ toRowMajorOrder $ boxmap (unbundle . fmap combineRegion . bundle) masks

    rows = gridFromRows . fmap repeat $ row_masks
    columns = gridFromRows . repeat $ col_masks
    boxes = fromBoxes . fmap repeat $ fromRowMajorOrder box_masks

    combine m1 m2 m3 = m1 .<>. m2 .<>. m3

    failed = (any_row_failed .||. any_col_failed .||. any_box_failed)

    any_row_failed = fold @(n * m - 1) (.||.) row_failed
    any_col_failed = fold @(n * m - 1) (.||.) col_failed
    any_box_failed = fold @(n * m - 1) (.||.) box_failed

combineRegion :: (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 2 <= n * m) => Vec (n * m) (Mask n m) -> (Bool, Mask n m)
combineRegion = foldr f (False, mempty)
  where
    f mask (failed, mask_acc) = (failed || overlapping mask mask_acc, mask <> mask_acc)

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

    (any_failed, neighbours_masks) = neighboursMasks masks

    units :: Grid n m (Signals dom (CellUnit n m))
    units = pure unit <*> shift_ins <*> prev_guesses <*> pops <*> neighbours_masks

    (_shift_out, shift_ins) = shiftInGridAtN (enable (isJust <$> shift_in) . cell <$> units) shift_in
    (_guessing_failed, prev_guesses) = shiftInGridAtN (keep_guessing <$> units) (pure True)

    masks = mask <$> units
    cells = cell <$> units
    conts = cont <$> units

    fresh = isJust <$> shift_in .||. isJust <$> pop
    all_unique = bitToBool . reduceAnd <$> bundle (is_unique <$> units)
    any_changed = bitToBool . reduceOr <$> bundle (changed <$> units)
    can_guess = not <$> all_unique

    result =
        mux fresh (pure Progress) $
        mux any_failed (pure Failure) $
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
        :: Signal dom (Maybe (Cell n m))
        -> Signal dom Bool
        -> Signal dom (Maybe (Cell n m))
        -> Signal dom (Mask n m)
        -> Signals dom (CellUnit n m)
    unit shift_in try_guess pop neighbours_mask = CellUnit{..}
      where
        shift_out = enable (isJust <$> shift_in) cell

        cell = register conflicted cell'
        mask = mux is_unique (cellMask <$> cell) (pure mempty)
        propagated = act <$> neighbours_mask <*> cell

        cell' = do
            shift_in <- shift_in
            pop <- pop
            can_propagate <- enable_propagate .&&. not <$> is_unique
            use_guess <- enable_guess .&&. guess_this
            guess <- first_guess
            propagated <- propagated
            old <- cell
            pure if
                | Just load <- shift_in -> load
                | Just load <- pop      -> load
                | use_guess             -> guess
                | can_propagate         -> propagated
                | otherwise             -> old

        changed = cell' ./=. cell

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_unique = next_guess .== conflicted

        can_guess = not <$> is_unique
        guess_this = try_guess .&&. can_guess
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. is_unique
