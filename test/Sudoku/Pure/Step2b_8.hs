{-# LANGUAGE RecordWildCards #-}
module Sudoku.Pure.Step2b_8 where

import Clash.Prelude hiding (mapAccumR)

import Sudoku.Solve (Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid
import Sudoku.Utils

import Data.Monoid.Action
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

data Result 
    = Blocked
    | Complete 
    | Progress 
    | Stuck

solve :: (KnownNat n, KnownNat m) => Sudoku n m -> (Result, Sudoku n m, Sudoku n m)
solve grid = (result, grid', next_guess)
  where
    result
        | blocked   = Blocked
        | complete  = Complete 
        | changed   = Progress 
        | otherwise = Stuck

    grid'
        | complete = grid
        | changed = pruned
        | otherwise = first_guess
    
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and singles

    consistent = not . bitsOverlap . fmap maskBits

    (singles, first_guess, next_guess) = expand grid
    pruned = apply <$> singles <*> group_masks <*> grid

    masks = maskOf <$> singles <*> grid
    group_masks = foldGroups masks
    changed = pruned /= grid

    maskOf single cell = if single then cellMask cell else mempty
    apply single mask cell = if single then cell else act mask cell

type Stack n m = [Sudoku n m]

sudoku :: forall n m f. (Alternative f, KnownNat n, KnownNat m) => Sudoku n m -> f (Sudoku n m)
sudoku = go emptyStack
  where
    go stack grid = case solve grid of
        (Complete, _, _) -> pure grid
        (Progress, grid',_) -> go stack grid'
        (Blocked, _, _) -> case pop stack of
            Nothing -> empty
            Just (top, stack') -> go stack' top
        (Stuck, grid1, grid2) -> go (push grid2 stack) grid1

    emptyStack = []
    push x xs = x:xs
    pop [] = Nothing
    pop (x:xs) = Just (x, xs)
