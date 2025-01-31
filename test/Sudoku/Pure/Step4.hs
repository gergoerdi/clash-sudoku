-- Keep iterating pruning until it doesn't change anything

module Sudoku.Pure.Step4 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku)
import Sudoku.Cell
import Sudoku.Grid

import Control.Monad (guard, (<=<), MonadPlus)
import Control.Monad.State.Strict

single :: (Solvable n m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

expand :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
expand grid = sequenceA $ evalState (traverse (state . guess1) grid) False
  where
    guess1 cell guessed_before
        | not guessed_before
        , (first_guess, next_guess) <- splitCell cell
        , next_guess /= conflicted
        = ([first_guess, next_guess], True)

        | otherwise
        = ([cell], guessed_before)

complete :: (Solvable n m) => Sudoku n m -> Bool
complete = all single

search :: (MonadPlus f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
search grid
    | complete grid           = pure grid
    | otherwise               = asum [sudoku grid' | grid' <- expand grid ]

prune :: (MonadPlus f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
prune grid = do
    guard $ safe && all (/= conflicted) grid
    pure $ act <$> group_masks <*> is_singles <*> grid
  where
    is_singles = single <$> grid
    masks = cellMask <$> is_singles <*> grid
    group_masks = foldGroups masks
    safe = allGroups consistent masks

consistent :: (Solvable n m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

loopM :: (Eq a, Monad m) => (a -> m a) -> a -> m a
loopM act x = do
    x' <- act x
    let changed = x' /= x
    if changed then loopM act x' else pure x'

sudoku :: (MonadPlus f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
sudoku = search <=< loopM prune
