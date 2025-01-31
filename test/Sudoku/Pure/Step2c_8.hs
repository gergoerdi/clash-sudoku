-- Open `sudoku`'s recursion
{-# LANGUAGE RecordWildCards #-}
module Sudoku.Pure.Step2c_8 where

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
    | Complete
    | Progress (Sudoku n m)
    | Stuck (Sudoku n m)

solve :: (KnownNat n, KnownNat m) => Sudoku n m -> (Result n m, Sudoku n m)
solve grid = (result, next_guess)
  where
    result
        | blocked   = Blocked
        | complete  = Complete
        | changed   = Progress pruned
        | otherwise = Stuck first_guess

    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and singles

    consistent = not . bitsOverlap . fmap maskBits

    (singles, first_guess, next_guess) = expand grid

    masks = cellMask <$> singles <*> grid
    group_masks = foldGroups masks

    pruned = act <$> group_masks <*> singles <*> grid
    changed = pruned /= grid

type Stack n m = [Sudoku n m]

sudoku' :: (Alternative f, KnownNat n, KnownNat m) => Sudoku n m -> f (Sudoku n m)
sudoku' = go
  where
    go grid =
      let (result, next_guess) = solve grid
      in case result of
        Blocked           -> empty
        Complete          -> pure grid
        Progress pruned   -> go pruned
        Stuck first_guess -> go first_guess <|> go next_guess

sudoku :: (Alternative f, KnownNat n, KnownNat m) => Sudoku n m -> f (Sudoku n m)
sudoku grid = go (grid, emptyStack)
  where
    go (grid, stack) =
        let (result, next_guess) = solve grid
        in case result of
            Complete -> pure grid
            Progress grid' -> go (grid', stack)
            Blocked -> case pop stack of
                Just (grid', stack') -> go (grid', stack')
                Nothing -> empty
            Stuck first_guess -> go (first_guess, push next_guess stack)

    emptyStack = []
    push x xs = x:xs
    pop [] = Nothing
    pop (x:xs) = Just (x, xs)
