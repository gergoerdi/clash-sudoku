{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve where

import Clash.Prelude

import Sudoku.Matrix
import Sudoku.Grid
import Sudoku.Stack

import Control.Arrow (second, (***))
import Data.Maybe

data Result n m
    = Working
    | Solution (Sudoku n m)
    | Unsolvable
    deriving (Generic, NFDataX)

data Step n m
    = Load (Sudoku n m)
    | Propagate
    deriving (Generic, NFDataX)

foldGrid :: (n * m * m * n ~ k + 1) => (a -> a -> a) -> Grid n m a -> a
foldGrid f = fold f . flattenGrid

unzipMatrix :: Matrix n m (a, b) -> (Matrix n m a, Matrix n m b)
unzipMatrix = (FromRows *** FromRows) . unzip . fmap unzip . matrixRows

unzipGrid :: Grid n m (a, b) -> (Grid n m a, Grid n m b)
unzipGrid = (Grid *** Grid) . unzipMatrix . fmap unzipMatrix . getGrid

propagator
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, n * m * m * n ~ k + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Step n m)
    -> ( Signal dom (Sudoku n m)
       , Signal dom Bool
       , Signal dom Bool
       )
propagator step = (bundle $ fmap (\(r, _, _) -> r) cells, changed, solved)
  where
    cells :: Grid n m (Signal dom (Cell n m), Signal dom Bool, Signal dom Bool)
    cells = generateGrid cell

    solved = foldGrid (liftA2 (.&.)) . fmap (\ (_, solved, _) -> solved) $ cells
    changed = foldGrid (liftA2 (.|.)) . fmap (\ (_, _, changed) -> changed) $ cells

    cell :: Coord n m -> (Signal dom (Cell n m), Signal dom Bool, Signal dom Bool)
    cell idx@(i, j, k, l) = (r, isUnique <$> r, register False changed)
      where
        r :: Signal dom (Cell n m)
        r = register conflicted r'

        (r', changed) = unbundle do
            step <- step
            this <- r
            row_masks <- traverse neighbour row'
            col_masks <- traverse neighbour col'
            square_masks <- traverse neighbour square'
            pure $ case step of
                Load grid -> (gridAt grid idx, True)
                Propagate -> (this', this' /= this)
                  where
                    new = combine this (row_masks ++ col_masks ++ square_masks)
                    this' | new /= conflicted = new
                          | otherwise = this

        neighbour idx = mux is_unique value (pure conflicted)
          where
            (value, is_unique, _) = gridAt cells idx

        row, col, square :: Vec (n * m) (Coord n m)
        row = concatMap (\k -> map (\l -> (i, j, k, l)) indicesI) indicesI
        col = concatMap (\i -> map (\j -> (i, j, k, l)) indicesI) indicesI
        square = concatMap (\j -> map (\l -> (i, j, k, l)) indicesI) indicesI

        row', col', square' :: Vec ((n * m) - 1) (Coord n m)
        row' = unconcatI (others row) !!! k !!! l
        col' = unconcatI (others col) !!! i !!! j
        square' = unconcatI (others square) !!! j !!! l

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
    (grid, changed, solved) = propagator step

    plan = second (unflattenGrid @n @m) . mapAccumL f False . flattenGrid <$> grid
      where
        f :: Bool -> Cell n m -> (Bool, (Cell n m, Cell n m))
        f found s | not found , Just (s', s'') <- splitCell s = (True, (s', s''))
                  | otherwise                              = (found, (s, s))
    (can_try, nextAfter) = unbundle plan
    (next, after) = unbundle . fmap unzipGrid $ nextAfter

    step =
        mux (changed .||. solved) (pure Propagate) $
        mux can_try (Load <$> next) $
        maybe Propagate Load <$> load

    stack_cmd =
        mux (changed .||. solved) (pure Nothing) $
        mux can_try (Just . Push <$> after) $
        mux (isJust <$> load) (pure Nothing) $
        pure $ Just Pop

others :: (1 <= n) => Vec n a -> Vec n (Vec (n - 1) a)
others (Cons x Nil) = Nil :> Nil
others (Cons x xs@(Cons _ _)) = xs :> map (x :>) (others xs)
