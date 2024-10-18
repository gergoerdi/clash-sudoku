{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure where

import Clash.Prelude

import Sudoku.Solve
import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid (Any(..), All(..))
import Data.Monoid.Action
import Control.Monad (guard)

propagate1 :: forall n m. (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
propagate1 grid = do
    masks' <- neighbourhoodMasks masks
    pure $ apply <$> uniques <*> masks' <*> grid
  where
    uniques = isUnique <$> grid
    masks = maskOf <$> uniques <*> grid

    isUnique cell = cell == fst (splitCell cell)
    maskOf is_unique cell = if is_unique then cellMask cell else mempty
    apply is_unique mask = if is_unique then id else act mask

propagate :: forall n m. (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
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
        , cells@(_:_:_) <- possibilities cell
        = (False, cells)

        | otherwise
        = (keep_guessing, [cell])

choices :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
choices = traverse possibilities

correct :: forall n m. (Solvable n m) => Sudoku n m -> Bool
correct = getAll . reduceAll . neighbourhoodwise noDups
  where
    noDups :: Vec (n * m) (Cell n m) -> All
    noDups xs = All $ all (`elem` xs) (unique <$> [minBound..maxBound])

failed :: forall n m. (Solvable n m) => Sudoku n m -> Bool
failed = getAny . reduceAny . fmap (== conflicted)

search :: forall n m. (Solvable n m) => Sudoku n m -> [Sudoku n m]
search grid = do
    guard $ not (failed grid)
    grid <- maybeToList $ propagate grid
    if correct grid then pure grid else search =<< guess grid
