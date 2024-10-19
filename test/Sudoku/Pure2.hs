{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}
module Sudoku.Pure2 where

import Clash.Prelude hiding (fold)

import Sudoku.Solve
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid.Action
import Control.Monad (guard)

data CellUnit n m = CellUnit
    { cell :: Cell n m
    , mask :: Mask n m
    , is_unique :: Bool
    , is_conflicted :: Bool
    , possibilities :: [Cell n m]
    }

unit :: (Solvable n m) => Cell n m -> Mask n m -> CellUnit n m
unit cell neighbourhood_mask = CellUnit{ cell = cell', ..}
  where
    (guess, cont) = splitCell cell
    is_unique = cont == conflicted
    is_conflicted = cell == conflicted
    possibilities = if is_unique then [cell] else [guess, cont]

    mask = if is_unique then cellMask cell else mempty
    cell' = if is_unique then cell else act neighbourhood_mask cell

propagate1 :: (Solvable n m) => Sudoku n m -> Maybe (Grid n m (CellUnit n m))
propagate1 grid = do
    rec
        let units = unit <$> grid <*> neighbourhood_masks
            masks = mask <$> units
        neighbourhood_masks <- neighbourhoodMasks masks
    guard $ not $ any is_conflicted units
    pure units

propagate :: (Solvable n m) => Sudoku n m -> Maybe (Grid n m (CellUnit n m))
propagate grid = do
    units <- propagate1 grid
    let grid' = cell <$> units
        changed = grid' /= grid
    if changed then propagate grid' else pure units

guess :: forall n m. (Solvable n m) => Grid n m (CellUnit n m) -> [Sudoku n m]
guess units = do
    guard guessed
    traverse snd xs
  where
    xs = f <$> units <*> prev_guesses

    (not -> guessed, prev_guesses) = shiftInGridAtN (fst <$> xs) True

    f :: CellUnit n m -> Bool -> (Bool, [Cell n m])
    f unit keep_guessing
        | keep_guessing
        , cells@(_:_:_) <- possibilities unit
        = (False, cells)

        | otherwise
        = (keep_guessing, [cell unit])

search :: forall n m. (Solvable n m) => Sudoku n m -> [Sudoku n m]
search grid = do
    units <- maybeToList $ propagate grid
    let grid' = cell <$> units
        correct = all is_unique units
    if correct then pure grid' else guess units >>= search
