{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve (controller) where

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

data PropagatorResult
    = Solved
    | Failure
    | Stuck
    | Guess
    | Progress
    deriving (Generic, NFDataX, Eq, Show)

propagator
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom PropagatorResult
       , Grid n m (Signal dom (Cell n m))
       , Signal dom Bool
       , Grid n m (Signal dom (Cell n m))
       )
propagator shift_in pop = (result, grid, can_guess, next_guesses)
  where
    pops :: Grid n m (Signal dom (Maybe (Cell n m)))
    pops = unbundle . fmap sequenceA $ pop

    ((shift_out, keep_guessing), unzip3 -> (unflattenGrid -> grid, changeds, unflattenGrid -> next_guesses)) =
        mapAccumR unit (shift_in, should_guess) (flattenGrid $ ((,) <$> pops <*> neighbourMasks grid uniques))

    uniques = fmap (isUnique <$>) grid
    faileds = fmap (.== conflicted) grid

    should_guess = not <$> any_changed
    can_guess = should_guess .&&. (not <$> keep_guessing)

    fresh = register False $ isJust <$> shift_in .||. isJust <$> pop
    all_unique = foldGrid (.&&.) uniques
    any_changed = fold (.||.) changeds
    any_failed  = foldGrid (.||.) faileds

    result =
        mux fresh (pure Progress) $
        mux all_unique (pure Solved) $
        mux any_failed (pure Failure) $
        mux can_guess (pure Guess) $
        mux should_guess (pure Stuck) $
        pure Progress

    unit
        :: forall k. (KnownNat k)
        => (Signal dom (Maybe (Cell n m)), Signal dom Bool)
        -> (Signal dom (Maybe (Cell n m)), Vec k (Signal dom (Mask n m)))
        -> ((Signal dom (Maybe (Cell n m)), Signal dom Bool), (Signal dom (Cell n m), Signal dom Bool, Signal dom (Cell n m)))
    unit (shift_in, try_guess) (pop, neighbour_masks) = ((shift_out, keep_guessing), (r, changed, cont))
      where
        load = shift_in .<|>. pop
        shift_out = enable (isJust <$> shift_in) r

        r = register conflicted r'
        r' = load .<|>. enable guess_this first_guess .<|. propagate r neighbour_masks
        changed = register False $ r ./=. r'

        (first_guess, next_guess) = unbundle $ mux try_guess (splitCell <$> r) (bundle (r, r))

        can_guess = next_guess ./= conflicted
        guess_this = try_guess .&&. can_guess
        cont = mux guess_this next_guess r
        keep_guessing = try_guess .&&. (not <$> guess_this)

controller
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Maybe (Sudoku n m))
       , Signal dom (Maybe (StackCmd (Sudoku n m)))
       )
controller new_board = (enable (result .== Solved) (bundle grid), stack_cmd)
  where
    (result, grid, can_guess, next_guesses) = propagator (pure Nothing) load
    load = new_board -- .<|>. enable (can_try .&&. register False (result .== Stuck)) next

    stack_cmd = do
        result <- result
        can_guess <- can_guess
        next_guesses <- bundle next_guesses
        pure $ case result of
            Solved -> Nothing
            Guess -> Just $ Push next_guesses
            Stuck -> Just Pop
            Failure -> Just Pop
            Progress -> Nothing
