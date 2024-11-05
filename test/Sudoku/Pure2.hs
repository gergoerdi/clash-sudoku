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
import Control.Monad.State.Strict

groupMasks :: (Solvable n m) => Grid n m (Mask n m) -> Maybe (Grid n m (Mask n m))
groupMasks masks = do
    guard $ allGroups consistent masks
    pure $ foldGroups masks

data CellUnit n m = CellUnit
    { cell :: Cell n m
    , mask :: Mask n m
    , is_unique :: Bool
    , is_conflicted :: Bool
    , possibilities :: [Cell n m]
    }

unit :: (Solvable n m) => Cell n m -> Mask n m -> CellUnit n m
unit cell group_mask = CellUnit{ cell = cell', ..}
  where
    (guess, cont) = splitCell cell
    is_unique = cont == conflicted
    is_conflicted = cell == conflicted
    possibilities = if is_unique then [cell] else [guess, cont]

    mask = if is_unique then cellMask cell else mempty
    cell' = if is_unique then cell else act group_mask cell

propagate1 :: (Solvable n m) => Sudoku n m -> Maybe (Grid n m (CellUnit n m))
propagate1 grid = do
    rec
        let units = unit <$> grid <*> group_masks
            masks = mask <$> units
        group_masks <- groupMasks masks
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
    let (cells', keep_guessing) = runState (traverse (state . guess1) units) True
    guard $ not keep_guessing
    sequenceA cells'
  where
    guess1 :: CellUnit n m -> Bool -> ([Cell n m], Bool)
    guess1 unit keep_guessing
        | keep_guessing
        , cells@(_:_:_) <- possibilities unit
        = (cells, False)

        | otherwise
        = ([cell unit], keep_guessing)

search :: forall n m. (Solvable n m) => Sudoku n m -> [Sudoku n m]
search grid = do
    units <- maybeToList $ propagate grid
    let grid' = cell <$> units
        correct = all is_unique units
    if correct then pure grid' else guess units >>= search
