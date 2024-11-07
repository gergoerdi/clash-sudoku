-- `expand` returns exactly two possible grid choices: the first
-- guess, and a continuation.
module Sudoku.Pure.Step2_5 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
import Control.Monad.State.Strict

single :: (Solvable n m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

expand :: (Solvable n m) => Sudoku n m -> (Sudoku n m, Sudoku n m)
expand grid = (grid1, grid2)
  where
    grids = evalState (traverse (state . guess1) grid) False
    (grid1, grid2) = (fst <$> grids, snd <$> grids)

    guess1 cell guessed_before
        | not guessed_before
        , (first_guess, next_guess) <- splitCell cell
        , next_guess /= conflicted
        = ((first_guess, next_guess), True)

        | otherwise
        = ((cell, cell), guessed_before)

sudoku :: (Alternative f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
sudoku grid
    | blocked   = empty
    | complete  = pure grid
    | otherwise = let (grid1, grid2) = expand pruned in sudoku grid1 <|> sudoku grid2
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

    maskOf is_single cell = if is_single then cellMask cell else mempty
    apply is_single mask = if is_single then id else act mask
