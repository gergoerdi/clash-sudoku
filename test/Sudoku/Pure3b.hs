{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure3b where

import Clash.Prelude hiding (fold)

import Sudoku.Solve
import Sudoku.Grid
import Sudoku.Cell

import Data.Monoid.Action
import Control.Monad ((<=<))
import Control.Monad.State.Strict
import Data.Maybe (maybeToList)

isUnique :: (Solvable n m) => Cell n m -> Bool
isUnique cell = popCount (cellBits cell) == 1

possibilities :: (Solvable n m) => Cell n m -> [Cell n m]
possibilities cell =
    [ cell'
    | i <- [minBound..maxBound]
    , let cell' = given i
    , cellBits cell .&. cellBits cell' /= 0
    ]

possibilities1 :: (Solvable n m) => Cell n m -> [Cell n m]
possibilities1 cell
    | cont == conflicted
    = [guess]

    | otherwise
    = [guess, cont]
  where
    (guess, cont) = splitCell cell

expand :: forall n m. (Solvable n m) => Grid n m (CellUnit n m) -> [Sudoku n m]
expand units = sequenceA $ evalState (traverse (state . guess1) units) False
  where
    guess1 unit guessed_before
        | not guessed_before
        , xs@(_:_:_) <- guesses unit
        = (xs, True)

        | otherwise
        = ([cell unit], guessed_before)

search :: (Solvable n m) => Grid n m (CellUnit n m) -> [Sudoku n m]
search units
    | any is_conflicted units
    = []

    | all isUnique cells
    = [cells]

    | otherwise
    = search <=< maybeToList . prune <=< expand $ units
  where
    cells = cell <$> units

data CellUnit n m = CellUnit
    { cell :: Cell n m
    , is_conflicted :: Bool
    , guesses :: [Cell n m]
    }

prune :: (Solvable n m) => Sudoku n m -> Maybe (Grid n m (CellUnit n m))
prune grid = do
    group_masks <- groupMasks masks
    pure $ apply <$> uniques <*> group_masks <*> grid
  where
    uniques = isUnique <$> grid
    masks = maskOf <$> uniques <*> grid

    maskOf is_unique cell = if is_unique then cellMask cell else mempty

    apply is_unique mask cell0 = CellUnit{..}
      where
        cell = if is_unique then cell0 else act mask cell0
        guesses = possibilities cell
        is_conflicted = cell == conflicted

solve' :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
solve' = search <=< maybeToList . prune
