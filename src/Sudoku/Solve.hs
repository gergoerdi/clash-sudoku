{-# LANGUAGE BlockArguments, ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve where

import Clash.Prelude

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
    -> ( Signal dom (Sudoku n m)
       , Signal dom (Maybe PropagatorResult)
       )
propagator load = (bundle $ fmap (\(c, _, _, _) -> c) units, result <$> solved <*> changed <*> failed)
  where
    units :: Grid n m (Signal dom (Cell n m), Signal dom Bool, Signal dom Bool, Signal dom Bool)
    units = generateGrid unit

    solved = foldGrid (liftA2 (.&.)) . fmap (\ (_, solved, _, _) -> solved) $ units
    changed = foldGrid (liftA2 (.|.)) . fmap (\ (_, _, changed, _) -> changed) $ units
    failed =  foldGrid (liftA2 (.|.)) . fmap (\ (_, _, _, failed) -> failed) $ units

    result solved changed failed
        | failed = Just Failure
        | solved = Just Success
        | not changed = Just Stuck
        | otherwise = Nothing

    unit :: Coord n m -> (Signal dom (Cell n m), Signal dom Bool, Signal dom Bool, Signal dom Bool)
    unit idx@(i, j, k, l) = (r, isUnique <$> r, register False changed, (== conflicted) <$> r)
      where
        r :: Signal dom (Cell n m)
        r = register conflicted r'

        (r', changed) = unbundle do
            load <- load
            this <- r
            row_masks <- traverse neighbour row'
            col_masks <- traverse neighbour col'
            box_masks <- traverse neighbour box'
            pure $ case load of
                Just new_grid -> (gridAt new_grid idx, True)
                Nothing -> (this', this' /= this)
                  where
                    this' = combine this (row_masks ++ col_masks ++ box_masks)

        neighbour idx = mux is_unique value (pure conflicted)
          where
            (value, is_unique, _, _) = gridAt units idx

        row, col, box :: Vec (n * m) (Coord n m)
        row = concatMap (\k -> map (\l -> (i, j, k, l)) indicesI) indicesI
        col = concatMap (\i -> map (\j -> (i, j, k, l)) indicesI) indicesI
        box = concatMap (\j -> map (\l -> (i, j, k, l)) indicesI) indicesI

        row', col', box' :: Vec ((n * m) - 1) (Coord n m)
        row' = unconcatI (others row) !!! k !!! l
        col' = unconcatI (others col) !!! i !!! j
        box' = unconcatI (others box) !!! j !!! l

controller
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Sudoku n m)
       , Signal dom Bool
       , Signal dom (Maybe (StackCmd (Sudoku n m)))
       )
controller load = (grid, solved, stack_cmd)
  where
    (grid, result) = propagator step

    plan = second (unflattenGrid @n @m) . mapAccumL f False . flattenGrid <$> grid
      where
        f :: Bool -> Cell n m -> (Bool, (Cell n m, Cell n m))
        f found s | not found , Just (s', s'') <- splitCell s = (True, (s', s''))
                  | otherwise                              = (found, (s, s))

    (step, stack_cmd, solved) = unbundle $ f <$> result <*> grid <*> load <*> plan
      where
        f result grid load (can_try, unzipGrid -> (next, after)) = case result of
            Nothing -> (Nothing, Nothing, False)
            Just Success -> (Nothing, Nothing, True)
            Just Stuck | can_try -> (Just next, Just $ Push after, False)
            Just Failure
                | Just grid' <- load -> (Just grid', Nothing, False)
                | otherwise -> (Nothing, Just Pop, False)

others :: (1 <= n) => Vec n a -> Vec n (Vec (n - 1) a)
others (Cons x Nil) = Nil :> Nil
others (Cons x xs@(Cons _ _)) = xs :> map (x :>) (others xs)
