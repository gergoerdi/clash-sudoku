{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure3 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku, neighbourhoodMasks)
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
    , let cell' = unique i
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

expand :: (Solvable n m) => (Cell n m -> [Cell n m]) -> Sudoku n m -> [Sudoku n m]
expand possibilities grid = sequenceA $ evalState (traverse (state . guess1) grid) False
  where
    guess1 x guessed_before
        | not guessed_before
        , xs@(_:_:_) <- possibilities x
        = (xs, True)

        | otherwise
        = ([x], guessed_before)

search :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
search grid
    | any (== conflicted) grid
    = []

    | any isUnique grid
    = [grid]

    | otherwise
    = sudoku =<< expand possibilities grid

prune :: (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
prune grid = do
    neighbourhood_masks <- neighbourhoodMasks masks
    pure $ apply <$> uniques <*> neighbourhood_masks <*> grid
  where
    uniques = isUnique <$> grid
    masks = maskOf <$> uniques <*> grid

    maskOf is_unique cell = if is_unique then cellMask cell else mempty
    apply is_unique mask = if is_unique then id else act mask

sudoku :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
sudoku = search <=< maybeToList . prune
