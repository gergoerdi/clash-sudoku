{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve (propagator, PropagatorResult(..)) where

import Clash.Prelude hiding (mapAccumR)
import RetroClash.Utils

import Sudoku.Hacks
import Sudoku.Matrix
import Sudoku.Grid
import Sudoku.Stack

import Control.Arrow (second, (***))
import Data.Maybe

import Debug.Trace

foldGrid :: (n * m * m * n ~ k + 1) => (a -> a -> a) -> Grid n m a -> a
foldGrid f = fold f . flattenGrid

unzipMatrix :: Matrix n m (a, b) -> (Matrix n m a, Matrix n m b)
unzipMatrix = (FromRows *** FromRows) . unzip . fmap unzip . matrixRows

unzipGrid :: Grid n m (a, b) -> (Grid n m a, Grid n m b)
unzipGrid = (Grid *** Grid) . unzipMatrix . fmap unzipMatrix . getGrid

data PropagatorResult
    = Solved
    | Failure
    | Guess
    | Progress
    deriving (Generic, NFDataX, Eq, Show)

neighbourMasks
    :: forall n m dom. (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 1 <= n * m)
    => (HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Cell n m))
    -> Grid n m (Signal dom Bool)
    -> Grid n m (Vec (3 * (n * m - 1)) (Signal dom (Mask n m)))
neighbourMasks cells uniques =
    rowwise others masks .++. columnwise others masks .++. boxwise others masks
  where
    masks = cellMask <$> cells <*> uniques

    cellMask :: Signal dom (Cell n m) -> Signal dom Bool -> Signal dom (Mask n m)
    cellMask c is_unique = mux is_unique (Mask . complement . cellBits <$> c) (pure wildMask)

    (.++.) = liftA2 (++)
    infixr 5 .++.

propagate
    :: forall n m dom k. (KnownNat n, KnownNat m, KnownNat k)
    => (HiddenClockResetEnable dom)
    => Signal dom (Cell n m)
    -> Vec k (Signal dom (Mask n m))
    -> Signal dom (Cell n m)
propagate cell neighbour_masks = applyMasks <$> cell <*> bundle neighbour_masks

propagator
    :: forall n m dom. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= n * m * m * n)
    => (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Cell n m)
       , Signal dom PropagatorResult
       , Grid n m (Signal dom (Cell n m))
       , Signal dom Bool
       , Grid n m (Signal dom (Cell n m))
       )
propagator enable_propagate commit_guess shift_in pop = (head (flattenGrid grid), result, grid, can_guess, next_guesses)
  where
    pops :: Grid n m (Signal dom (Maybe (Cell n m)))
    pops = unbundle . fmap sequenceA $ pop

    ((_shift_out, keep_guessing), unzip4 -> (unflattenGrid -> grid, unflattenGrid -> uniques, changeds, unflattenGrid -> next_guesses)) =
        mapAccumR unit (shift_in, should_guess) (flattenGrid $ ((,) <$> pops <*> neighbourMasks grid uniques))

    -- uniques = fmap (isUnique <$>) grid
    faileds = fmap (.== conflicted) grid

    should_guess = not <$> any_changed
    can_guess = should_guess .&&. (not <$> keep_guessing)

    fresh = register False $ isJust <$> shift_in .||. isJust <$> pop
    all_unique = foldGrid (.&&.) uniques
    any_changed = fold @(n * m * m * n - 1) (.||.) changeds
    any_failed  = foldGrid (.||.) faileds

    result =
        mux fresh (pure Progress) $
        mux all_unique (pure Solved) $
        mux any_failed (pure Failure) $
        mux can_guess (pure Guess) $
        mux should_guess (pure Failure) $
        pure Progress

    unit
        :: forall k. (KnownNat k)
        => (Signal dom (Maybe (Cell n m)), Signal dom Bool)
        -> (Signal dom (Maybe (Cell n m)), Vec k (Signal dom (Mask n m)))
        -> ((Signal dom (Maybe (Cell n m)), Signal dom Bool), (Signal dom (Cell n m), Signal dom Bool, Signal dom Bool, Signal dom (Cell n m)))
    unit (shift_in, try_guess) (pop, neighbour_masks) = ((shift_out, keep_guessing), (r, unique, changed, cont))
      where
        load = shift_in .<|>. pop
        shift_out = enable (isJust <$> shift_in) r

        r = register conflicted r'
        r' = load .<|>. enable (commit_guess .&&. guess_this) first_guess .<|>. enable enable_propagate (propagate r neighbour_masks) .<|. r
        changed = register False $ r ./=. r'
        unique = isUnique <$> r

        (first_guess, next_guess) = unbundle $ mux try_guess (splitCell <$> r) (bundle (r, r))

        can_guess = not <$> unique
        guess_this = enable_propagate .&&. try_guess .&&. can_guess
        cont = mux guess_this next_guess r
        keep_guessing = try_guess .&&. (not <$> guess_this)
