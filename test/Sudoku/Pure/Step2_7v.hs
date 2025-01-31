-- Open `sudoku`'s recursion with a sized stack
{-# LANGUAGE RecordWildCards #-}
module Sudoku.Pure.Step2_7v where

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

data Result n m
    = Blocked
    | Complete (Sudoku n m)
    | Progress (Sudoku n m)
    | Stuck (Sudoku n m) (Sudoku n m)

solve :: (KnownNat n, KnownNat m) => Sudoku n m -> Result n m
solve grid
    | blocked   = Blocked
    | complete  = Complete grid
    | changed   = Progress pruned
    | otherwise = Stuck grid1 grid2
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

type StackDepth n m = ((n * m) * (m * n))
type StackMem n m = Vec (StackDepth n m) (Sudoku n m)
type StackPtr n m = Index (StackDepth n m)
type Stack n m = (StackMem n m, StackPtr n m)

sudoku :: forall n m f. (Alternative f, KnownNat n, KnownNat m, 1 <= StackDepth n m) => Sudoku n m -> f (Sudoku n m)
sudoku = go emptyStack
  where
    go :: Stack n m -> Sudoku n m -> f (Sudoku n m)
    go stack grid = case solve grid of
        Complete solution -> pure solution
        Progress grid' -> go stack grid'
        Blocked -> case pop stack of
            Nothing -> empty
            Just (top, stack') -> go stack' top
        Stuck grid1 grid2 -> go (push grid2 stack) grid1

    emptyStack = (repeat undefined, 0)
    push x (elems, sp) = (replace sp x elems, sp + 1)
    pop (elems, sp)
        | sp == 0
        = Nothing

        | otherwise
        = let sp' = sp - 1 in Just (elems !! sp', (elems, sp'))
