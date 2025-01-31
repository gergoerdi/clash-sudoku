-- `expand` returns exactly two possible grid choices: the first
-- guess, and a continuation.
module Sudoku.Pure.Step2_5 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku)
import Sudoku.Cell
import Sudoku.Grid

import Control.Monad.State.Strict

single :: (Solvable n m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

expand :: (Solvable n m) => Sudoku n m -> (Sudoku n m, Sudoku n m)
expand grid = (fst <$> grids, snd <$> grids)
  where
    grids = evalState (traverse (state . guess1) grid) False

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
    | changed   = sudoku pruned
    | otherwise = let (grid1, grid2) = expand grid in sudoku grid1 <|> sudoku grid2
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and is_singles

    consistent = not . bitsOverlap . fmap maskBits

    is_singles = single <$> grid
    masks = cellMask <$> is_singles <*> grid
    group_masks = foldGroups masks

    pruned = act <$> group_masks <*> is_singles <*> grid
    changed = pruned /= grid
