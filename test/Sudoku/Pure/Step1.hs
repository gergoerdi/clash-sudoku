-- Initial version: a close translation of Bird '06, using the
-- hardware-friendly `Cell n m` representation for cells and the
-- nicely size-indexed `Grid n m` representation for Sudoku grids.

module Sudoku.Pure.Step1 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
import Control.Monad.State.Strict

single :: (Solvable n m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

choices :: (Solvable n m) => Cell n m -> [Cell n m]
choices cell = [ unique i | i <- [minBound..maxBound], cellBits cell ! i == 1 ]

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
safe = allNeighbourhoods consistent

consistent :: (Solvable n m, KnownNat k) => Vec k (Cell n m) -> Bool
consistent = not . bitsOverlap . fmap (\cell -> if single cell then cellBits cell else 0)

search :: (Alternative f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
search grid
    | not (safe grid)           = empty
    | any (== conflicted) grid = empty
    | complete grid           = pure grid
    | otherwise               = asum [sudoku grid' | grid' <- expand grid]

prune :: (Solvable n m) => Sudoku n m -> Sudoku n m
prune grid = apply <$> is_singles <*> neighbourhood_masks <*> grid
  where
    is_singles = single <$> grid
    masks = maskOf <$> is_singles <*> grid
    neighbourhood_masks = foldNeighbourhoods masks

    maskOf is_single cell = if is_single then cellMask cell else mempty
    apply is_single mask = if is_single then id else act mask

sudoku :: (Alternative f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
sudoku = search . prune
