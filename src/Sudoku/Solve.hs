{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve where

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

data PropagatorResult
    = Failure
    | Stuck
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
    :: forall n m dom k l. (KnownNat n, KnownNat m, KnownNat k)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Cell n m))
    -> Signal dom (Cell n m)
    -> Vec k (Signal dom (Mask n m))
    -> Signal dom (Cell n m)
propagate load cell neighbour_masks = do
    load <- load
    cell <- cell
    masks <- bundle neighbour_masks
    pure $ case load of
        Just new_cell -> new_cell
        Nothing -> applyMasks cell masks

propagator
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom PropagatorResult
       , Grid n m (Signal dom (Cell n m))
       , Signal dom Bool
       )
propagator load = (result, grid, all_unique)
  where
    loads :: Grid n m (Signal dom (Maybe (Cell n m)))
    loads = unbundle . fmap sequenceA $ load

    units@(unzipGrid -> (grid, changeds)) = unit <$> loads <*> neighbourMasks grid uniques
    uniques = fmap (isUnique <$>) grid
    faileds = fmap (.== conflicted) grid

    fresh = register False $ isJust <$> load
    all_unique = foldGrid (.&&.) uniques
    any_changed = foldGrid (.||.) changeds
    any_failed  = foldGrid (.||.) faileds

    result =
        mux fresh (pure Progress) $
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
        r' = propagate load r neighbour_masks
        changed = register False $ r ./=. r'

controller
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Maybe (Sudoku n m))
       , Signal dom (Maybe (StackCmd (Sudoku n m)))
       )
controller load = (enable solved (bundle grid), stack_cmd)
  where
    (result, grid, solved) = propagator step

    (can_try, unzipGrid -> (bundle -> next, bundle -> after)) = second unflattenGrid . mapAccumL f (pure False) . flattenGrid $ grid
      where
        f :: Signal dom Bool -> Signal dom (Cell n m) -> (Signal dom Bool, (Signal dom (Cell n m), Signal dom (Cell n m)))
        f found c = (found .||. this, unbundle $ mux this guess (dup <$> c))
          where
            guess@(unbundle -> (_next, after)) = splitCell <$> c
            this = (not <$> found) .&&. (after ./= conflicted)
            dup x = (x, x)

    step = load .<|>. enable (can_try .&&. register False (result .== Stuck)) next
    stack_cmd = do
        result <- result
        can_try <- can_try
        after <- after
        pure $ case result of
            Stuck -> Just $ if can_try then Push after else Pop
            Failure -> Just Pop
            Progress -> Nothing
