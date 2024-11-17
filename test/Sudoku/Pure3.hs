{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure3 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
import Control.Monad.State.Strict

isUnique :: (Solvable n m) => Cell n m -> Bool
isUnique cell = popCount (cellBits cell) == 1

possibilities :: (Solvable n m) => Cell n m -> [Cell n m]
possibilities cell =
    [ cell'
    | i <- [minBound..maxBound]
    , let cell' = given i
    , cellBits cell .&. cellBits cell' /= 0
    ]

possibilities1 :: (Solvable n m) => Cell n m -> [Cell n m]
possibilities1 cell
    | cont == conflicted
    = [guess]

    | otherwise
    = [guess, cont]
  where
    (guess, cont) = splitCell cell

expand :: (Solvable n m) => (Cell n m -> [Cell n m]) -> Sudoku n m -> [Sudoku n m]
expand possibilities grid = sequenceA $ evalState (traverse (state . guess1) grid) False
  where
    guess1 x guessed_before
        | not guessed_before
        , xs@(_:_:_) <- possibilities x
        = (xs, True)

        | otherwise
        = ([x], guessed_before)

complete :: (Solvable n m) => Sudoku n m -> Bool
complete = all isUnique

blocked :: (Solvable n m) => Sudoku n m -> Bool
blocked grid = any (== conflicted) grid || not (safe grid)

safe :: (Solvable n m) => Sudoku n m -> Bool
safe = allGroups consistent

consistent :: (Solvable n m, KnownNat k) => Vec k (Cell n m) -> Bool
consistent = not . bitsOverlap . fmap (\cell -> if isUnique cell then cellBits cell else 0)

search :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
search grid
    | not (safe grid)           = empty
    | any (== conflicted) grid = empty
    | complete grid           = pure grid
    | otherwise               = sudoku =<< expand possibilities grid

prune :: (Solvable n m) => Sudoku n m -> Sudoku n m
prune grid = apply <$> uniques <*> group_masks <*> grid
  where
    uniques = isUnique <$> grid
    masks = maskOf <$> uniques <*> grid
    group_masks = foldGroups masks

    maskOf is_unique cell = if is_unique then cellMask cell else mempty
    apply is_unique mask = if is_unique then id else act mask

fix :: (Eq a) => (a -> a) -> (a -> a)
fix f x = let x' = f x in if x == x' then x' else f x'

sudoku :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
sudoku = search . prune
