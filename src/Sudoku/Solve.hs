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
    | Success
    | Stuck
    deriving (Generic, NFDataX, Eq, Show)

propagator
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> ( Grid n m (Signal dom (Cell n m), Signal dom Bool)
       , Signal dom (Maybe PropagatorResult)
       )
propagator load = (fmap (\(c, solved, _, _) -> (c, solved)) units, result)
  where
    units :: Grid n m (Signal dom (Cell n m), Signal dom Bool, Signal dom Bool, Signal dom Bool)
    units = generateGrid unit

    solved = foldGrid (liftA2 (.&.)) . fmap (\ (_, solved, _, _) -> solved) $ units
    changed = foldGrid (liftA2 (.|.)) . fmap (\ (_, _, changed, _) -> changed) $ units
    failed =  foldGrid (liftA2 (.|.)) . fmap (\ (_, _, _, failed) -> failed) $ units

    result = do
        fresh <- register False $ isJust <$> load
        solved <- solved
        changed <- changed
        failed <- failed
        pure $ if
            | fresh     -> Nothing
            | failed    -> Just Failure
            | solved    -> Just Success
            | not changed -> Just Stuck
            | otherwise -> Nothing

    unit :: Coord n m -> (Signal dom (Cell n m), Signal dom Bool, Signal dom Bool, Signal dom Bool)
    unit idx = (r, isUnique <$> r, register False changed, (== conflicted) <$> r)
      where
        r :: Signal dom (Cell n m)
        r = register conflicted r'

        (r', changed) = unbundle do
            load <- load
            this <- r
            masks <- traverse neighbour (neighbours idx)
            pure $ case load of
                Just new_grid -> (gridAt new_grid idx, True)
                Nothing -> (this', this' /= this)
                  where
                    this' = applyMasks this masks

        neighbour idx' = mux is_unique mask (pure wildMask)
          where
            (value, is_unique, _, _) = gridAt units idx'
            mask = Mask . complement . cellBits <$> value

controller
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Sudoku n m)
       , Signal dom Bool
       , Signal dom (Maybe (StackCmd (Sudoku n m)))
       )
controller load = (bundle . fmap fst $ grid, solved, stack_cmd)
  where
    (grid, result) = propagator step

    (can_try, unzipGrid -> (next, after)) = second unflattenGrid . mapAccumL f (pure False) . flattenGrid $ grid
      where
        f found (s, solved) = (found .||. this, unbundle $ mux this (splitCell <$> s) (dup <$> s))
          where
            this = (not <$> found) .&&. (not <$> solved)
            dup x = (x, x)

    step = load .<|>. enable (can_try .&&. result .== Just Stuck) (bundle next)
    solved = result .== Just Success
    stack_cmd =
        mux (result .== Just Stuck .&&. can_try) (Just . Push <$> bundle after) $
        mux (result .== Just Failure) (pure $ Just Pop) $
        pure Nothing
