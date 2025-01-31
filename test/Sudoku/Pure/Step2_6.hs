-- Compute `single` via `splitCell`
module Sudoku.Pure.Step2_6 where

import Clash.Prelude hiding (mapAccumR)

import Sudoku.Solve (Sudoku)
import Sudoku.Cell
import Sudoku.Grid
import Sudoku.Utils

import Data.Traversable

expand :: (KnownNat n, KnownNat m) => Sudoku n m -> (Grid n m Bool, Sudoku n m, Sudoku n m)
expand = funzip3 . snd . mapAccumR guess False
  where
    guess guessed_before cell
        | not guessed_before && not single
        = (True, (single, first_guess, next_guess))

        | otherwise
        = (guessed_before, (single, cell, cell))
      where
        (first_guess, next_guess) = splitCell cell
        single = next_guess == conflicted

sudoku :: (Alternative f, KnownNat n, KnownNat m) => Sudoku n m -> f (Sudoku n m)
sudoku grid
    | blocked   = empty
    | complete  = pure grid
    | changed   = sudoku pruned
    | otherwise = sudoku grid1 <|> sudoku grid2
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and singles

    consistent = not . bitsOverlap . fmap maskBits

    (singles, grid1, grid2) = expand grid

    masks = cellMask <$> singles <*> grid
    group_masks = foldGroups masks

    pruned = act <$> group_masks <*> singles <*> grid
    changed = pruned /= grid
