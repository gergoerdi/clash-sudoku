-- Compute `single` via `splitCell`
{-# LANGUAGE RecordWildCards #-}
module Sudoku.Pure.Step2_7 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
import Control.Monad.State.Strict

data CellUnit n m = CellUnit
    { cell :: Cell n m
    , single :: Bool
    , first_guess, next_guess :: Cell n m
    }

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
    complete = all single units
    
    consistent = not . bitsOverlap . fmap maskBits

    units = unit <$> grid
    unit cell = CellUnit{..}
      where
        (first_guess, next_guess) = splitCell cell
        single = next_guess == conflicted

    masks = maskOf <$> units
    group_masks = foldGroups masks
    
    pruned = apply <$> group_masks <*> units 
    changed = pruned /= grid

    maskOf CellUnit{..} = if single then cellMask cell else mempty
    apply mask CellUnit{..} = if single then cell else act mask cell

    guesses = evalState (traverse (state . guess1) units) False
    (grid1, grid2) = (fst <$> guesses, snd <$> guesses)

    guess1 CellUnit{..} guessed_before
        | not guessed_before
        , not single
        = ((first_guess, next_guess), True)
    
        | otherwise
        = ((cell, cell), guessed_before)

