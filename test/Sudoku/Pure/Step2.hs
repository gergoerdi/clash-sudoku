-- After `prune`, we immediately check `safe` so we might as well do
-- it inside `prune`. This avoids recomputing `isUnique` in `consistent`.

module Sudoku.Pure.Step2 where

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

choices :: (Solvable n m) => Cell n m -> [Cell n m]
choices cell =
    [ cell'
    | i <- [minBound..maxBound]
    , let cell' = unique i
    , cellBits cell .&. cellBits cell' /= 0
    ]

expand :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
expand grid = sequenceA $ evalState (traverse (state . guess1) grid) False
  where
    guess1 cell guessed_before
        | not guessed_before
        , cells@(_:_:_) <- choices cell
        = (cells, True)

        | otherwise
        = ([cell], guessed_before)

complete :: (Solvable n m) => Sudoku n m -> Bool
complete = all isUnique

search :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
search grid
    | any (== conflicted) grid = empty
    | complete grid           = pure grid
    | otherwise               = sudoku =<< expand grid

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

sudoku :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
sudoku = search <=< maybeToList . prune
