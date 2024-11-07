-- Compute `single` via `splitCell`
module Sudoku.Pure.Step2_6 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
import Control.Monad.State.Strict

sudoku :: (Alternative f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
sudoku grid
    | blocked   = empty
    | complete  = pure grid
    | changed   = sudoku pruned
    | otherwise = sudoku grid1 <|> sudoku grid2
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and is_singles
    
    consistent = not . bitsOverlap . fmap maskBits

    splits = (\c -> (c, splitCell c)) <$> grid

    is_singles = (\(c, (first_guess, next_guess)) -> next_guess == conflicted) <$> splits
    masks = maskOf <$> is_singles <*> grid
    group_masks = foldGroups masks
    
    pruned = apply <$> is_singles <*> group_masks <*> grid
    changed = pruned /= grid

    maskOf is_single cell = if is_single then cellMask cell else mempty
    apply is_single mask = if is_single then id else act mask

    guesses = evalState (traverse (state . guess1) splits) False
    (grid1, grid2) = (fst <$> guesses, snd <$> guesses)

    guess1 (cell, (first_guess, next_guess)) guessed_before
        | not guessed_before
        , next_guess /= conflicted
        = ((first_guess, next_guess), True)

        | otherwise
        = ((cell, cell), guessed_before)

