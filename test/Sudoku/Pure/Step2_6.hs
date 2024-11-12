-- Compute `single` via `splitCell`
module Sudoku.Pure.Step2_6 where

import Clash.Prelude hiding (mapAccumR)

import Sudoku.Solve (Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
import Data.Traversable

expand :: (KnownNat n, KnownNat m) => Sudoku n m -> (Grid n m Bool, Sudoku n m, Sudoku n m)
expand grid = (singles, first_guesses, next_guesses)
  where
    (_, guesses) = mapAccumR guess False grid
    singles = fst <$> guesses
    first_guesses = fst . snd <$> guesses
    next_guesses = snd . snd <$> guesses

    guess guessed_before cell
        | not guessed_before && not single
        = (True, (single, split))

        | otherwise
        = (guessed_before, (single, (cell, cell)))
      where
        split@(_, next_guess) = splitCell cell
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

    masks = maskOf <$> singles <*> grid
    group_masks = foldGroups masks
    
    pruned = apply <$> singles <*> group_masks <*> grid
    changed = pruned /= grid

    maskOf single cell = if single then cellMask cell else mempty
    apply single mask = if single then id else act mask
