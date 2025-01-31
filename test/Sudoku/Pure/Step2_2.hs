-- Merge `prune` into `search`
module Sudoku.Pure.Step2_2 where

import Clash.Prelude hiding (mapAccumR)

import Sudoku.Solve (Sudoku)
import Sudoku.Cell
import Sudoku.Grid

import Data.Traversable (mapAccumR)

single :: (KnownNat n, KnownNat m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

choices :: (KnownNat n, KnownNat m) => Cell n m -> [Cell n m]
choices cell = [ given i | i <- [minBound..maxBound], cell `canBe` i ]

expand :: (KnownNat n, KnownNat m) => Sudoku n m -> [Sudoku n m]
expand = sequenceA . snd . mapAccumR guess1 False
  where
    guess1 guessed_before cell
        | not guessed_before
        , cells@(_:_:_) <- choices cell
        = (True, cells)

        | otherwise
        = (guessed_before, [cell])

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
    masks = cellMask <$> is_singles <*> grid
    group_masks = foldGroups masks

    pruned = act <$> group_masks <*> is_singles <*> grid
    changed = pruned /= grid
