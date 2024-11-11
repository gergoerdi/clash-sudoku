-- Merge `prune` into `search`
module Sudoku.Pure.Step2_3 where

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

consistent :: (Solvable n m, KnownNat k) => Vec k (Cell n m) -> Bool
consistent = not . bitsOverlap . fmap (\cell -> if single cell then cellBits cell else 0)

sudoku :: (Alternative f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
sudoku grid
    | blocked   = empty
    | complete  = pure grid
    | changed   = sudoku pruned
    | otherwise = asum [sudoku grid' | grid' <- expand pruned]
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent grid
    complete = all single grid
    
    pruned = apply <$> group_masks <*> grid
    changed = pruned /= grid

    masks = maskOf <$> grid
    group_masks = foldGroups masks
    
    maskOf cell = if single cell then cellMask cell else mempty
    apply mask cell = if single cell then cell else act mask cell
