-- Compute `single` via `splitCell`
module Sudoku.Pure.Step2_6 where

import Clash.Prelude hiding (mapAccumL)

import Sudoku.Solve (Solvable, Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
-- import Control.Monad.State.Strict
import Data.Traversable

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
    complete = and singles
    
    consistent = not . bitsOverlap . fmap maskBits

    splits = splitCell <$> grid

    singles = (== conflicted) . snd <$> splits
    masks = maskOf <$> singles <*> grid
    group_masks = foldGroups masks
    
    pruned = apply <$> singles <*> group_masks <*> grid
    changed = pruned /= grid

    maskOf single cell = if single then cellMask cell else mempty
    apply single mask = if single then id else act mask

    (_, guesses) = mapAccumL guess1 False $ (,,) <$> singles <*> grid <*> splits
    (grid1, grid2) = (fst <$> guesses, snd <$> guesses)

    guess1 guessed_before (single, cell, (first_guess, next_guess))
        | not guessed_before
        , not single
        = (True, (first_guess, next_guess))

        | otherwise
        = (guessed_before, (cell, cell))

