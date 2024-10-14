{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , shiftInGridAtN
    , neighbourhoodMasks

    , propagator
    , PropagatorCmd(..)
    , PropagatorResult(..)
    ) where

import Clash.Prelude hiding (fold)
import Clash.Num.Overflowing

import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid (Any(..), All(..))
import Data.Monoid.Action
import Data.Foldable (fold)

shiftInGridAtN :: forall n m a. (KnownNat n, KnownNat m) => Grid n m a -> a -> (a, Grid n m a)
shiftInGridAtN grid x = (x', unflattenGrid grid')
  where
    (grid', x' :> Nil) = shiftInAtN (flattenGrid grid) (x :> Nil)

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

neighbourhoodMasks :: forall n m. (Solvable n m) => Grid n m (Mask n m) -> (Bool, Grid n m (Mask n m))
neighbourhoodMasks masks = (overlap, masks')
  where
    masks' = neighbourhoodwise fold masks
    Any overlap = reduceAny $ neighbourhoodwise overlappingMasks masks

overlappingMasks :: forall n m k. (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Any
overlappingMasks = reduceAny . overlappingBits . fmap (complement . maskBits)

overlappingBits :: forall n k. (KnownNat n, KnownNat k) => Vec k (BitVector n) -> BitVector n
overlappingBits =
    v2bv . map boolToBit .
    map (hasOverflowed . sum . map toOverflowing) .
    transpose . map bv2v

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

    masks = bundle $ mask <$> units

    (overlapping_uniques, unbundle -> neighbours_masks) = unbundle . fmap neighbourhoodMasks $ masks

    units :: Grid n m (CellUnit dom n m)
    units = pure unit <*> shift_ins <*> prev_guesses <*> pops <*> neighbours_masks

    (_, shift_ins) = shiftInGridAtN (enable (isJust <$> shift_in) . cell <$> units) shift_in
    (_, prev_guesses) = shiftInGridAtN (keep_guessing <$> units) (pure True)

    all_unique  = getAll . reduceAll <$> bundle (is_unique <$> units)
    any_changed = getAny . reduceAny <$> bundle (changed <$> units)
    any_failed  = getAny . reduceAny <$> bundle (is_conflicted <$> units)

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

        changed = cell' ./=. cell

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_unique = next_guess .==. pure conflicted
        is_conflicted = cell .==. pure conflicted

        can_guess = not <$> is_unique
        guess_this = try_guess .&&. can_guess
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. is_unique
