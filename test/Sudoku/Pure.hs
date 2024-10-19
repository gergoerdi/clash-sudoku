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
    = []

    | otherwise
    = [guess, cont]
  where
    (guess, cont) = splitCell cell

guess :: forall n m. (Solvable n m) => Sudoku n m -> [Sudoku n m]
guess cells = do
    guard guessed
    traverse snd units
  where
    units = f <$> cells <*> prev_guesses

    (not -> guessed, prev_guesses) = shiftInGridAtN (fst <$> units) True

    f :: Cell n m -> Bool -> (Bool, [Cell n m])
    f cell keep_guessing
        | keep_guessing
        , cells@(_:_:_) <- possibilities1 cell
        = (False, cells)

        | otherwise
        = (keep_guessing, [cell])

choices :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
choices = traverse possibilities

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
