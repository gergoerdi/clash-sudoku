{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure where

import Clash.Prelude hiding (fold)

import Sudoku.Solve
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid (All(..))
import Data.Monoid.Action
import Data.Foldable (fold)
import Control.Monad (guard)
import Control.Monad.State.Strict

isUnique :: (Solvable n m) => Cell n m -> Bool
isUnique cell = popCount (cellBits cell) == 1

propagate1 :: (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
propagate1 grid = do
    guard $ not (failed grid)
    masks' <- neighbourhoodMasks masks
    pure $ apply <$> uniques <*> masks' <*> grid
  where
    uniques = isUnique <$> grid
    masks = maskOf <$> uniques <*> grid

    maskOf is_unique cell = if is_unique then cellMask cell else mempty
    apply is_unique mask = if is_unique then id else act mask

propagate :: (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
propagate grid = do
    grid' <- propagate1 grid
    let changed = grid' /= grid
    if changed then propagate grid' else pure grid

possibilities :: (Solvable n m) => Cell n m -> [Cell n m]
possibilities cell =
    [ cell'
    | i <- [minBound..maxBound]
    , let cell' = unique i
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

guessFirst :: (Traversable f) => (a -> [a]) -> f a -> [f a]
guessFirst possibilities struct = do
    let (structs, guessed) = runState (traverse (state . guess1) struct) False
    guard guessed
    sequenceA structs
  where
    guess1 x guessed_before
        | not guessed_before
        , xs@(_:_:_) <- possibilities x
        = (xs, True)

        | otherwise
        = ([x], guessed_before)

guess :: forall n m. (Solvable n m) => Sudoku n m -> [Sudoku n m]
guess = guessFirst possibilities1

correct :: forall n m. (Solvable n m) => Sudoku n m -> Bool
correct = getAll . fold . neighbourhoodwise noDups
  where
    noDups :: Vec (n * m) (Cell n m) -> All
    noDups xs = All $ all (`elem` xs) (unique <$> [minBound..maxBound])

correct' :: forall n m. (Solvable n m) => Sudoku n m -> Bool
correct' = all isUnique

failed :: forall n m. (Solvable n m) => Sudoku n m -> Bool
failed = any (== conflicted)

search :: forall n m. (Solvable n m) => Sudoku n m -> [Sudoku n m]
search grid = do
    grid <- maybeToList $ propagate grid
    if correct grid then pure grid else search =<< guess grid
