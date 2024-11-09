{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns, BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
module Sudoku.Show where

import Clash.Prelude hiding (lift)

import Data.Char (chr)

import Sudoku.Iso
import Sudoku.Grid
import Sudoku.Cell
import Format
import Format.Model

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

type GridFormat n m = ((((Forward :++ " ") :* n :++ " ") :* m :++ "\r\n") :* m :++ "\r\n") :* n

showGrid :: forall n m. (Textual n m) => Sudoku n m -> String
showGrid =
    fmap (chr . fromIntegral) .
    formatModel (GridFormat n m) .
    fmap showCell .
    toList . embed flatGrid

instance (Textual n m) => Show (Sudoku n m) where
    show = showGrid
