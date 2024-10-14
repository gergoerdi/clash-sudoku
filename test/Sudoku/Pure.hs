{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure where

import Clash.Prelude hiding (fold)
import qualified Data.List as L

import Sudoku.Solve
import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid (Any(..), All(..))
import Data.Monoid.Action
import Control.Monad (guard)

propagate1 :: forall n m. (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
propagate1 cells = do
    guard $ not (failed cells)
    guard $ not overlap
    pure cells'
  where
    isUnique cell = cell == fst (splitCell cell)
    uniques = isUnique <$> cells

    masks = (\cell is_unique -> if is_unique then cellMask cell else mempty) <$> cells <*> uniques
    (overlap, masks') = neighbourhoodMasks masks
    cells' = (\ is_unique mask -> if is_unique then id else act mask) <$> uniques <*> masks' <*> cells

propagate :: forall n m. (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
propagate cells = do
    cells' <- propagate1 cells
    if cells' == cells then pure cells else propagate cells'

possibilities :: (Solvable n m) => Cell n m -> [Cell n m]
possibilities cell =
    L.filter (\cell' -> cellBits cell .&. cellBits cell' /= 0) .
    L.map unique $
    [minBound..maxBound]

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
    grid <- maybeToList $ propagate grid
    if correct grid then pure grid else search =<< guess grid
