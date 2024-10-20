{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

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
import Data.Monoid (All(..))
import Data.Monoid.Action
import Data.Foldable (fold)
import Control.Monad (guard)
import Control.Monad.State.Strict

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

neighbourhoodMasks :: forall n m. (Solvable n m) => Grid n m (Mask n m) -> Maybe (Grid n m (Mask n m))
neighbourhoodMasks masks = do
    guard safe
    pure masks'
  where
    masks' = neighbourhoodwise fold masks
    safe = getAll . fold $ neighbourhoodwise (All . safeMasks) masks

safeMasks :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Bool
safeMasks = (== 0) . overlappingBits . fmap (complement . maskBits)

overlappingBits :: (KnownNat n, KnownNat k) => Vec k (BitVector n) -> BitVector n
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
propagator cmd shift_in pop = (lastGrid (cell <$> units), result, bundle $ cont <$> units)
  where
    pops = unbundle . fmap sequenceA $ pop

    masks = bundle $ mask <$> units

    mb_neighbourhood_masks = neighbourhoodMasks <$> masks
    neighbourhood_masks = unbundle . fmap sequenceA $ mb_neighbourhood_masks
    overlapping_uniques = isNothing <$> mb_neighbourhood_masks

    units :: Grid n m (CellUnit dom n m)
    units = evalState (traverse (state . uncurry unit) ((,) <$> pops <*> neighbourhood_masks)) (shift_in, pure False)

    all_unique  = and <$> bundle (is_unique <$> units)
    any_changed = or <$> bundle (changed <$> units)
    any_failed  = or <$> bundle (is_conflicted <$> units)

    result =
        mux overlapping_uniques  (pure Failure) $
        mux any_failed           (pure Failure) $
        mux all_unique           (pure Solved) $
        mux any_changed          (pure Progress) $
        pure Stuck

    unit
        :: Signal dom (Maybe (Cell n m))
        -> Signal dom (Maybe (Mask n m))
        -> (Signal dom (Maybe (Cell n m)), Signal dom Bool)
        -> (CellUnit dom n m, (Signal dom (Maybe (Cell n m)), Signal dom Bool))
    unit pop neighbourhood_mask (shift_in, guessed_before) = (CellUnit{..}, (shift_out, guessed))
      where
        cell = register conflicted cell'

        shift_out = enable (isJust <$> shift_in) cell

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_unique = next_guess .==. pure conflicted
        is_conflicted = cell .==. pure conflicted

        mask = mux is_unique (cellMask <$> cell) (pure mempty)

        can_guess_this = not <$> is_unique
        guess_this = can_guess_this .&&. not <$> guessed_before
        cont = mux guess_this next_guess cell
        guessed = guessed_before .||. can_guess_this

        cell' = do
            current <- cell
            shift_in <- shift_in
            pop <- pop
            cmd <- cmd
            guess_this <- guess_this
            is_unique <- is_unique
            first_guess <- first_guess
            neighbourhood_mask <- neighbourhood_mask
            pure if
                | Just load <- shift_in                                                  -> load
                | Just load <- pop                                                       -> load
                | Just Propagate <- cmd, not is_unique, Just mask <- neighbourhood_mask  -> act mask current
                | Just CommitGuess <- cmd, guess_this                                    -> first_guess
                | otherwise                                                              -> current

        changed = cell' ./=. cell
