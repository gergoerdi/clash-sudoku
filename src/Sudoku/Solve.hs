{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , neighbourhoodMasks
    , bitsOverlap
    , consistent

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
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n, 1 <= n * m)

neighbourhoodMasks :: forall n m. (Solvable n m) => Grid n m (Mask n m) -> Maybe (Grid n m (Mask n m))
neighbourhoodMasks masks = do
    guard safe
    pure masks'
  where
    masks' = neighbourhoodwise fold masks
    safe = getAll . fold $ neighbourhoodwise (All . consistent) masks

consistent :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

bitsOverlap :: (KnownNat n, KnownNat k) => Vec k (BitVector n) -> Bool
bitsOverlap = any (hasOverflowed . sum . map toOverflowing) . transpose . map bv2v

neighbourhoodMaskBits :: forall n m. (Solvable n m) => Grid n m MaskBit -> Maybe (Grid n m MaskBit)
neighbourhoodMaskBits masks = do
    guard safe
    pure masks'
  where
    masks' = neighbourhoodwise fold masks
    safe = getAll . fold $ neighbourhoodwise (All . safeMaskBits) masks

safeMaskBits :: (KnownNat k) => Vec k MaskBit -> Bool
safeMaskBits = not . hasOverflowed . sum . map (toOverflowing . maskBit)

data CellUnit dom n m = CellUnit
    { cell :: Signal dom (Cell n m)
    , mask :: Signal dom MaskBit
    , is_single :: Signal dom Bool
    , is_conflicted :: Signal dom Bool
    , changed :: Signal dom Bool
    , cont :: Signal dom (Cell n m)
    }

data PropagatorCmd
    = Rotate
    | Propagate
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
    -> Grid n m (Signal dom (Maybe (Cell n m)))
    -> ( Signal dom (Cell n m)
       , Signal dom PropagatorResult
       , Grid n m (Signal dom (Cell n m))
       )
propagator cmd shift_in pops = (lastGrid (cell <$> units), result, cont <$> units)
  where
    masks = bundle $ mask <$> units

    mb_neighbourhood_masks = neighbourhoodMaskBits <$> masks
    neighbourhood_masks = unbundle . fmap sequenceA $ mb_neighbourhood_masks
    overlapping_singles = isNothing <$> mb_neighbourhood_masks

    units :: Grid n m (CellUnit dom n m)
    units = evalState (traverse (state . uncurry unit) ((,) <$> pops <*> neighbourhood_masks)) (shift_in, pure False)

    all_single  = and <$> bundle (is_single <$> units)
    any_changed = or <$> bundle (changed <$> units)
    any_failed  = or <$> bundle (is_conflicted <$> units)

    result =
        mux overlapping_singles  (pure Failure) $
        mux any_failed           (pure Failure) $
        mux all_single           (pure Solved) $
        mux any_changed          (pure Progress) $
        pure Stuck

    unit
        :: Signal dom (Maybe (Cell n m))
        -> Signal dom (Maybe MaskBit)
        -> (Signal dom (Maybe (Cell n m)), Signal dom Bool)
        -> (CellUnit dom n m, (Signal dom (Maybe (Cell n m)), Signal dom Bool))
    unit pop neighbourhood_mask (shift_in, guessed_before) = (CellUnit{..}, (shift_out, guessed))
      where
        cell = register conflicted cell'

        shift_out = enable (isJust <$> shift_in) cell

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_single = next_guess .==. pure conflicted
        is_conflicted = cell .==. pure conflicted

        mask = mux is_single (cellMaskBit <$> cell) (pure mempty)

        can_guess_this = not <$> is_single
        guess_this = can_guess_this .&&. not <$> guessed_before
        cont = mux guess_this next_guess cell
        guessed = guessed_before .||. can_guess_this

        cell' = do
            current <- cell
            shift_in <- shift_in
            pop <- pop
            cmd <- cmd
            guess_this <- guess_this
            is_single <- is_single
            first_guess <- first_guess
            neighbourhood_mask <- neighbourhood_mask
            pure if
                | Just load <- shift_in <|> pop                                          -> load
                | Just Propagate <- cmd, not is_single, Just mask <- neighbourhood_mask  -> act mask current
                | Just CommitGuess <- cmd, guess_this                                    -> first_guess
                | Just Rotate <- cmd                                                     -> rotateCell current
                | otherwise                                                              -> current

        changed = cell' ./=. cell
