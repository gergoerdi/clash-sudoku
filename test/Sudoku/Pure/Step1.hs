-- Initial version: a close translation of Bird '06, using the
-- hardware-friendly `Cell n m` representation for cells and the
-- nicely size-indexed `Grid n m` representation for Sudoku grids.

module Sudoku.Pure.Step1 where

import Clash.Prelude hiding (fold)

import Sudoku.Solve (Solvable, Sudoku, overlappingBits)
import Sudoku.Cell
import Sudoku.Grid

import Data.Foldable (fold)
import Data.Monoid (All(..))
import Data.Monoid.Action
import Control.Monad.State.Strict

single :: (Solvable n m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

choices :: (Solvable n m) => Cell n m -> [Cell n m]
choices cell =
    [ cell'
    | i <- [minBound..maxBound]
    , let cell' = unique i
    , cellBits cell .&. cellBits cell' /= 0
    ]

expand :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
expand grid = sequenceA $ evalState (traverse (state . guess1) grid) False
  where
    guess1 cell guessed_before
        | not guessed_before
        , cells@(_:_:_) <- choices cell
        = (cells, True)

        | otherwise
        = ([cell], guessed_before)

complete :: (Solvable n m) => Sudoku n m -> Bool
complete = all single

safe :: (Solvable n m) => Sudoku n m -> Bool
safe = getAll . fold . neighbourhoodwise consistent

consistent :: (Solvable n m, KnownNat k) => Vec k (Cell n m) -> All
consistent = All . (== 0) . overlappingBits . fmap (\cell -> if single cell then cellBits cell else 0)

search :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
search grid
    | not (safe grid)           = empty
    | any (== conflicted) grid = empty
    | complete grid           = pure grid
    | otherwise               = sudoku =<< expand grid

prune :: (Solvable n m) => Sudoku n m -> Sudoku n m
prune grid = apply <$> is_singles <*> neighbourhood_masks <*> grid
  where
    is_singles = single <$> grid
    masks = maskOf <$> is_singles <*> grid
    neighbourhood_masks = neighbourhoodwise fold masks

    maskOf is_single cell = if is_single then cellMask cell else mempty
    apply is_single mask = if is_single then id else act mask

sudoku :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
sudoku = search . prune
