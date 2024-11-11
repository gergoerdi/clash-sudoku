-- Merge `prune` into `search`
module Sudoku.Pure.Step2_2 where

import Clash.Prelude

import Sudoku.Solve (Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Data.Monoid.Action
import Control.Monad.State.Strict

single :: (KnownNat n, KnownNat m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

choices :: (KnownNat n, KnownNat m) => Cell n m -> [Cell n m]
choices cell = [ given i | i <- [minBound..maxBound], cellBits cell ! i == 1 ]

expand :: (KnownNat n, KnownNat m) => Sudoku n m -> [Sudoku n m]
expand grid = sequenceA $ evalState (traverse (state . guess1) grid) False
  where
    guess1 cell guessed_before
        | not guessed_before
        , cells@(_:_:_) <- choices cell
        = (cells, True)

        | otherwise
        = ([cell], guessed_before)

complete :: (KnownNat n, KnownNat m) => Sudoku n m -> Bool
complete = all single

safe :: (KnownNat n, KnownNat m) => Sudoku n m -> Bool
safe = allGroups consistent

consistent :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Cell n m) -> Bool
consistent = not . bitsOverlap . fmap (\cell -> if single cell then cellBits cell else 0)

sudoku :: (Alternative f, KnownNat n, KnownNat m) => Sudoku n m -> f (Sudoku n m)
sudoku grid
    | not (safe grid)           = empty
    | any (== conflicted) grid = empty
    | complete grid           = pure grid
    | changed                 = sudoku pruned
    | otherwise               = asum [sudoku grid' | grid' <- expand grid]
  where
    is_singles = single <$> grid
    masks = maskOf <$> is_singles <*> grid
    group_masks = foldGroups masks
    
    pruned = apply <$> is_singles <*> group_masks <*> grid
    changed = pruned /= grid

    maskOf is_single cell = if is_single then cellMask cell else mempty
    apply is_single mask = if is_single then id else act mask

