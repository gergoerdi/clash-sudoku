{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , bitsOverlap
    , consistent

    , propagator
    , PropagatorCmd(..)
    , PropagatorResult(..)
    ) where

import Clash.Prelude
import Clash.Num.Overflowing

import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid.Action
import Control.Monad.State.Strict
import Data.Monoid (Ap(..))

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

consistent :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

bitsOverlap :: (KnownNat n, KnownNat k) => Vec k (BitVector n) -> Bool
bitsOverlap = any (hasOverflowed . sum . map toOverflowing) . transpose . map bv2v

data CellUnit dom n m = CellUnit
    { cell :: Signal dom (Cell n m)
    , mask :: Signal dom (Mask n m)
    , is_single :: Signal dom Bool
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

propagate
    :: (Solvable n m, HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Mask n m))
    -> Grid n m (Signal dom (Mask n m))
propagate = fmap getAp . foldGroups . fmap Ap

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
    pops = unbundle . fmap sequenceA $ pop

    masks = mask <$> units

    group_masks = propagate masks
    safe = allGroups consistent <$> bundle masks

    units :: Grid n m (CellUnit dom n m)
    units = evalState (traverse (state . uncurry unit) ((,) <$> pops <*> group_masks)) (shift_in, pure False)

    all_single  = and <$> bundle (is_single <$> units)
    any_changed = or <$> bundle (changed <$> units)
    any_failed  = or <$> bundle (is_conflicted <$> units)

    result =
        mux (not <$> safe)  (pure Failure) $
        mux any_failed      (pure Failure) $
        mux all_single      (pure Solved) $
        mux any_changed     (pure Progress) $
        pure Stuck

    unit
        :: Signal dom (Maybe (Cell n m))
        -> Signal dom (Mask n m)
        -> (Signal dom (Maybe (Cell n m)), Signal dom Bool)
        -> (CellUnit dom n m, (Signal dom (Maybe (Cell n m)), Signal dom Bool))
    unit pop group_mask (shift_in, guessed_before) = (CellUnit{..}, (shift_out, guessed))
      where
        cell = register conflicted cell'

        shift_out = enable (isJust <$> shift_in) cell

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_single = next_guess .==. pure conflicted
        is_conflicted = cell .==. pure conflicted

        mask = mux is_single (cellMask <$> cell) (pure mempty)

        can_guess_this = not <$> is_single
        guess_this = can_guess_this .&&. not <$> guessed_before
        cont = mux guess_this next_guess cell
        guessed = guessed_before .||. can_guess_this

        update cmd load mask guess cell
            | Just load <- load                             = load
            | Just Propagate <- cmd, Just mask <- mask      = act mask cell
            | Just CommitGuess <- cmd, Just guess <- guess  = guess
            | otherwise                                     = cell

        cell' = update
            <$> cmd
            <*> (shift_in .<|>. pop)
            <*> enable (not <$> is_single) mask
            <*> enable guess_this first_guess
            <*> cell

        changed = cell' ./=. cell
