-- Avoid repeated `single` recomputation
module Sudoku.Pure.Step2_4 where

import Clash.Prelude hiding (mapAccumR)

import Sudoku.Solve (Solvable, Sudoku)
import Sudoku.Cell
import Sudoku.Grid

import Data.Traversable (mapAccumR)

single :: (Solvable n m) => Cell n m -> Bool
single cell = popCount (cellBits cell) == 1

choices :: (Solvable n m) => Cell n m -> [Cell n m]
choices cell = [ given i | i <- [minBound..maxBound], cell `canBe` i ]

expand :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
expand = sequenceA . snd . mapAccumR guess False
  where
    guess guessed_before cell
        | not guessed_before
        , cells@(_:_:_) <- choices cell
        = (True, cells)

        | otherwise
        = (guessed_before, [cell])

sudoku :: (Alternative f, Solvable n m) => Sudoku n m -> f (Sudoku n m)
sudoku grid
    | blocked   = empty
    | complete  = pure grid
    | changed   = sudoku pruned
    | otherwise = asum [sudoku grid' | grid' <- expand grid]
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and is_singles

    consistent = not . bitsOverlap . fmap maskBits

    is_singles = single <$> grid
    masks = cellMask <$> is_singles <*> grid
    group_masks = foldGroups masks

    pruned = act <$> group_masks <*> is_singles <*> grid
    changed = pruned /= grid
