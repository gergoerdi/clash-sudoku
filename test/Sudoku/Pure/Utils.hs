module Sudoku.Pure.Utils where

import Clash.Prelude hiding (fold, concatMap, toList, minimum, length)
import qualified Clash.Sized.Vector as V

import qualified Sudoku.Grid as Grid
import Sudoku.Iso

type Matrix a = [[a]]
type Board = Matrix Char

fromGrid :: (KnownNat n, KnownNat m) => Grid.Grid n m a -> Matrix a
fromGrid = V.toList . fmap V.toList . embed Grid.rows

toGrid :: Matrix a -> Grid.Grid 3 3 a
toGrid = project Grid.rows . V.unsafeFromList . fmap V.unsafeFromList
