-- Open `sudoku`'s recursion
{-# LANGUAGE RecordWildCards #-}
module Sudoku.Pure.Step2_7 where

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

data Result n m
    = Blocked
    | Complete
    | Progress (Sudoku n m)
    | Stuck (Sudoku n m) (Sudoku n m)

solve :: (KnownNat n, KnownNat m) => Sudoku n m -> Result n m
solve grid
    | blocked   = Blocked
    | complete  = Complete
    | changed   = Progress pruned
    | otherwise = Stuck grid1 grid2
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

type Stack n m = [Sudoku n m]

sudoku' :: (Alternative f, KnownNat n, KnownNat m) => Sudoku n m -> f (Sudoku n m)
sudoku' = go 
  where
    go grid = case solve grid of
        Complete  -> pure grid
        Progress grid' -> go grid'
        Blocked -> empty
        Stuck grid1 grid2 -> go grid1 <|> go grid2

sudoku :: (Alternative f, KnownNat n, KnownNat m) => Sudoku n m -> f (Sudoku n m)
sudoku grid = go (grid, emptyStack)
  where
    go (grid, stack) = case solve grid of
        Complete -> pure grid
        Progress grid' -> go (grid', stack)
        Blocked -> case pop stack of
            Just (grid', stack') -> go (grid', stack')
            Nothing -> empty
            -- >>= go
        Stuck grid1 grid2 -> go (grid1, push grid2 stack)

    emptyStack = []
    push x xs = x:xs
    pop [] = Nothing
    pop (x:xs) = Just (x, xs)
