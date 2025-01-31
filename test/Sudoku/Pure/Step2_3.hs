-- Merge `prune` into `search`
module Sudoku.Pure.Step2_3 where

import Clash.Prelude hiding (mapAccumR)

import Sudoku.Solve (Solvable, Sudoku)
import Sudoku.Cell
import Sudoku.Grid

import Data.Traversable (mapAccumR)

single :: (KnownNat n, KnownNat m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

choices :: (KnownNat n, KnownNat m) => Cell n m -> [Cell n m]
choices cell = [ given i | i <- [minBound..maxBound], cell `canBe` i ]

expand :: (KnownNat n, KnownNat m) => Sudoku n m -> [Sudoku n m]
expand = sequenceA . snd . mapAccumR guess False
  where
    guess guessed_before cell
        | not guessed_before
        , cells@(_:_:_) <- choices cell
        = (True, cells)

        | otherwise
        = (guessed_before, [cell])

consistent :: (Solvable n m, KnownNat k) => Vec k (Cell n m) -> Bool
consistent = not . bitsOverlap . fmap (\cell -> if single cell then cellBits cell else 0)

sudoku :: (Alternative f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
sudoku grid
    | blocked   = empty
    | complete  = pure grid
    | changed   = sudoku pruned
    | otherwise = asum [sudoku grid' | grid' <- expand pruned]
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent grid
    complete = all single grid

    pruned = apply <$> group_masks <*> grid
    changed = pruned /= grid

    masks = maskOf <$> grid
    group_masks = foldGroups masks

    maskOf cell = cellMask (single cell) cell
    apply mask cell = act mask (single cell) cell
