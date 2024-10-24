-- `expand` returns exactly two possible grid choices: the first
-- guess, and a continuation.

module Sudoku.Pure.Step5 where

import Clash.Prelude hiding (fold)

import Sudoku.Solve (Solvable, Sudoku, safeMasks)
import Sudoku.Cell
import Sudoku.Grid

import Data.Foldable (fold)
import Data.Monoid (All(..))
import Data.Monoid.Action
import Data.Maybe (maybeToList)
import Control.Monad (guard, (<=<))
import Control.Monad.State.Strict

isUnique :: (Solvable n m) => Cell n m -> Bool
isUnique cell = popCount (cellBits cell) == 1

expand :: (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m, Sudoku n m)
expand grid = do
    let (grids, guessed) = runState (traverse (state . guess1) grid) False
    guard guessed
    let (grid1, grid2) = (fst <$> grids, snd <$> grids)
    pure (grid1, grid2)
  where
    guess1 cell guessed_before
        | not guessed_before
        , (first_guess, next_guess) <- splitCell cell
        , next_guess /= conflicted
        = ((first_guess, next_guess), True)

        | otherwise
        = ((cell, cell), guessed_before)

complete :: (Solvable n m) => Sudoku n m -> Bool
complete = all isUnique

search :: (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
search grid
    | any (== conflicted) grid = empty
    | complete grid           = pure grid
    | otherwise               = do
          (grid1, grid2) <- expand grid
          sudoku grid1 <|> sudoku grid2

prune :: (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
prune grid = do
    guard safe
    pure $ apply <$> uniques <*> neighbourhood_masks <*> grid
  where
    uniques = isUnique <$> grid
    masks = maskOf <$> uniques <*> grid
    neighbourhood_masks = neighbourhoodwise fold masks
    safe = getAll . fold . neighbourhoodwise (All . safeMasks) $ masks

    maskOf is_unique cell = if is_unique then cellMask cell else mempty
    apply is_unique mask = if is_unique then id else act mask

loopM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
loopM act x = do
    x' <- act x
    let changed = x' /= x
    if changed then loopM act x' else pure x'

sudoku :: (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
sudoku = search <=< loopM prune
