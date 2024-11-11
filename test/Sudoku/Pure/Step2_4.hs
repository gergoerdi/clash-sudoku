-- Avoid repeated `single` recomputation
module Sudoku.Pure.Step2_4 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
import Control.Monad.State.Strict

single :: (Solvable n m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

choices :: (Solvable n m) => Cell n m -> [Cell n m]
choices cell = [ given i | i <- [minBound..maxBound], cellBits cell ! i == 1 ]

expand :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
expand grid = sequenceA $ evalState (traverse (state . guess1) grid) False
  where
    guess1 cell guessed_before
        | not guessed_before
        , cells@(_:_:_) <- choices cell
        = (cells, True)

        | otherwise
        = ([cell], guessed_before)

sudoku :: (Alternative f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
sudoku grid
    | blocked   = empty
    | complete  = pure grid
    | changed   = sudoku pruned
    | otherwise = asum [sudoku grid' | grid' <- expand grid]
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and is_singles
    
    consistent = not . bitsOverlap . fmap maskBits

    is_singles = single <$> grid
    masks = maskOf <$> is_singles <*> grid
    group_masks = foldGroups masks
    
    pruned = apply <$> is_singles <*> group_masks <*> grid
    changed = pruned /= grid

    maskOf is_single cell = if is_single then cellMask cell else mempty
    apply is_single mask = if is_single then id else act mask
