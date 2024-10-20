{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure3 where

import Clash.Prelude hiding (fold, concatMap, toList, minimum, length)
import Prelude (concatMap)

import Sudoku.Solve
import Sudoku.Grid
import Sudoku.Cell

import Data.Monoid (All(..))
import Data.Monoid.Action
import Data.Foldable (fold, toList, minimum, length)
import Control.Monad.State.Strict

isUnique :: (Solvable n m) => Cell n m -> Bool
isUnique cell = popCount (cellBits cell) == 1

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

expandFirst :: forall n m. (Solvable n m) => (Cell n m -> [Cell n m]) -> Sudoku n m -> [Sudoku n m]
expandFirst possibilities grid = sequenceA $ evalState (traverse (state . guess1) grid) False
  where
    guess1 x guessed_before
        | not guessed_before
        , xs@(_:_:_) <- possibilities x
        = (xs, True)

        | otherwise
        = ([x], guessed_before)

expandSmallest :: forall n m. (Solvable n m) => Sudoku n m -> [Sudoku n m]
expandSmallest grid = sequenceA $ evalState (traverse (state . guess1) grid') False
  where
    grid' = (\x -> (x, possibilities x)) <$> grid
    n = minimum . filter (> 1) . toList . fmap (length . snd) $ grid'

    guess1 (x, xs) guessed_before
        | not guessed_before
        , length xs == n
        = (xs, True)

        | otherwise
        = ([x], guessed_before)

correct :: forall n m. (Solvable n m) => Sudoku n m -> Bool
correct = getAll . fold . neighbourhoodwise noDups
  where
    noDups :: Vec (n * m) (Cell n m) -> All
    noDups xs = All $ all (`elem` xs) (unique <$> [minBound..maxBound])

correct' :: forall n m. (Solvable n m) => Sudoku n m -> Bool
correct' = all isUnique

search :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
search grid
    | any (== conflicted) grid = []
    | all isUnique grid       = [grid]
    | otherwise               = concatMap (search . prune) . expandFirst possibilities $ grid

prune :: (Solvable n m) => Sudoku n m -> Sudoku n m
prune grid =
    case neighbourhoodMasks masks of
        Nothing -> pure conflicted
        Just neighbourhood_masks ->
          apply <$> uniques <*> neighbourhood_masks <*> grid
  where
    uniques = isUnique <$> grid
    masks = maskOf <$> uniques <*> grid
    neighbourhood_masks = neighbourhoodwise fold masks

    maskOf is_unique cell = if is_unique then cellMask cell else mempty
    apply is_unique mask = if is_unique then id else act mask

solve' :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
solve' = search . prune
