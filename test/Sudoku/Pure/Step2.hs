-- After `prune`, we immediately check `safe` so we might as well do
-- it inside `prune`. This avoids recomputing `isUnique` in `consistent`.

module Sudoku.Pure.Step2 where

import Clash.Prelude

import Sudoku.Solve (Solvable, Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
import Control.Monad (guard, (<=<), MonadPlus)
import Control.Monad.State.Strict

single :: (Solvable n m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

choices :: (Solvable n m) => Cell n m -> [Cell n m]
choices cell = [ unique i | i <- [minBound..maxBound], cellBits cell ! i == 1 ]

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
complete = all single

search :: (MonadPlus f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
search grid
    | any (== conflicted) grid = empty
    | complete grid           = pure grid
    | otherwise               = asum [sudoku grid' | grid' <- expand grid ]

prune :: (MonadPlus f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
prune grid = do
    guard safe
    pure $ apply <$> is_singles <*> group_masks <*> grid
  where
    is_singles = single <$> grid
    masks = maskOf <$> is_singles <*> grid
    group_masks = foldGroups masks
    safe = allGroups consistent masks

    maskOf is_single cell = if is_single then cellMask cell else mempty
    apply is_single mask = if is_single then id else act mask

consistent :: (Solvable n m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

sudoku :: (MonadPlus f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
sudoku = search <=< prune
