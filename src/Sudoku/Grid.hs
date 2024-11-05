{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fconstraint-solver-iterations=5 #-}
module Sudoku.Grid where

import Clash.Prelude hiding ((.), imap, fold)
import Data.Functor.Compose
import Data.Monoid (Ap(..))
import Sudoku.Iso
import Control.Category ((.))
import Data.Foldable (fold)

import Sudoku.Matrix

newtype Grid n m a = Grid{ getGrid :: Matrix n m (Matrix m n a) }
    deriving stock (Generic, Eq)
    deriving anyclass (NFDataX)
    deriving (Functor, Applicative, Foldable) via Compose (Matrix n m) (Matrix m n)
    deriving (Semigroup, Monoid) via Ap (Grid n m) a

instance (KnownNat n, KnownNat m) => Bundle (Grid n m a) where
    type Unbundled dom (Grid n m a) = Grid n m (Signal dom a)

    bundle = fmap Grid . bundle . fmap bundle . getGrid
    unbundle = Grid . fmap unbundle . unbundle . fmap getGrid

instance (KnownNat n, KnownNat m) => Traversable (Grid n m) where
    traverse f = fmap (project phi) . traverse f . embed phi
      where
        phi = ireverse . flatGrid
        ireverse = Iso reverse reverse

flatGrid :: (KnownNat n, KnownNat m) => Grid n m a <-> Vec (n * m * m * n) a
flatGrid = iconcat . rows

headGrid :: forall n m a. (KnownNat n, KnownNat m, 1 <= n * m * m * n) => Grid n m a -> a
headGrid = head @(n * m * m * n - 1) . embed flatGrid

grid :: Grid n m a <-> Matrix n m (Matrix m n a)
grid = icoerce

transposeGrid :: (KnownNat n, KnownNat m) => Grid n m a <-> Grid m n a
transposeGrid = inv grid . imap transposeMatrix . transposeMatrix . grid

type Group n m a = Vec (m * n) a
type Groups n m a = Vec (n * m) (Group n m a)
type Grouping n m = forall a. Grid n m a <-> Groups n m a

rows, cols, boxs :: (KnownNat n, KnownNat m) => Grouping n m
rows = imap iconcat . iconcat . imap itranspose . matrix . imap matrix . grid
cols = rows . transposeGrid
boxs = rowMajorOrder . imap rowMajorOrder . grid

foldGroups :: (KnownNat n, KnownNat m, Monoid a) => Grid n m a -> Grid n m a
foldGroups = foldBy rows <> foldBy cols <> foldBy boxs
  where
    foldBy
        :: (KnownNat n, KnownNat m, Monoid a)
        => Grouping n m
        -> Grid n m a
        -> Grid n m a
    foldBy grouping = project grouping . fmap (repeat . fold) . embed grouping

allGroups :: (KnownNat n, KnownNat m) => (Group n m a -> Bool) -> Grid n m a -> Bool
allGroups p grid = allBy rows && allBy cols && allBy boxs
  where
    allBy group = all p $ embed group grid
