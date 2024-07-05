{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve (controller) where

import Clash.Prelude
import RetroClash.Utils

import Sudoku.Matrix
import Sudoku.Grid
import Sudoku.Stack

import Control.Arrow (second, (***))
import Data.Maybe

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
    | Progress
    deriving (Generic, NFDataX, Eq, Show)

propagator
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Maybe (Cell n m)))
    -> ( Signal dom PropagatorResult
       , Grid n m (Signal dom (Cell n m))
       )
propagator loads = (result, grid)
  where
    units@(unzipGrid -> (grid, changeds)) = unit <$> loads <*> neighbourMasks grid uniques
    uniques = fmap (isUnique <$>) grid
    faileds = fmap (.== conflicted) grid

    fresh = register False $ foldGrid (.||.) $ fmap (isJust <$>) loads
    all_unique = foldGrid (.&&.) uniques
    any_changed = foldGrid (.||.) changeds
    any_failed  = foldGrid (.||.) faileds

    result =
        mux fresh (pure Progress) $
        mux all_unique (pure Solved) $
        mux any_failed (pure Failure) $
        mux (not <$> any_changed) (pure Stuck) $
        pure Progress

    unit
        :: forall k. (KnownNat k)
        => Signal dom (Maybe (Cell n m))
        -> Vec k (Signal dom (Mask n m))
        -> (Signal dom (Cell n m), Signal dom Bool)
    unit load neighbour_masks = (r, changed)
      where
        r = register conflicted r'
        r' = load .<|. propagate r neighbour_masks
        changed = register False $ r ./=. r'

controller
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Maybe (Sudoku n m))
       , Signal dom (Maybe (StackCmd (Sudoku n m)))
       )
controller new_board = (enable (result .== Solved) (bundle grid), stack_cmd)
  where
    (result, grid) = propagator loads

    (can_try, unzipGrid -> (bundle -> next, bundle -> after)) = mapAccumGridB (liftA2 f) (pure False) grid
      where
        f :: Bool -> Cell n m -> (Bool, (Cell n m, Cell n m))
        f found c = (found || this, if this then guess else (c, c))
          where
            guess@(_next, after) = splitCell c
            this = not found && after /= conflicted

    load = new_board .<|>. enable (can_try .&&. register False (result .== Stuck)) next
    loads = unbundle . fmap sequenceA $ load

    stack_cmd = do
        result <- result
        can_try <- can_try
        after <- after
        pure $ case result of
            Solved -> Nothing
            Stuck -> Just $ if can_try then Push after else Pop
            Failure -> Just Pop
            Progress -> Nothing
