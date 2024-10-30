{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fconstraint-solver-iterations=5 #-}
module Sudoku.Grid where

import Clash.Prelude
import Data.Functor.Compose
import Data.Coerce

import Sudoku.Matrix

newtype Grid n m a = Grid{ getGrid :: Matrix n m (Matrix m n a) }
    deriving stock (Generic)
    deriving anyclass (NFDataX, BitPack)
    deriving newtype (Eq)
    deriving (Functor, Applicative) via Compose (Matrix n m) (Matrix m n)

-- More efficient than via Compose (Matrix n m) (Matrix m n)
instance (KnownNat n, KnownNat m) => Foldable (Grid n m) where
    foldMap f = foldMap f . flattenGrid

instance (KnownNat n, KnownNat m) => Bundle (Grid n m a) where
    type Unbundled dom (Grid n m a) = Grid n m (Signal dom a)

    bundle = fmap Grid . bundle . fmap bundle . getGrid
    unbundle = Grid . fmap unbundle . unbundle . fmap getGrid

instance (KnownNat n, KnownNat m) => Traversable (Grid n m) where
    traverse f = fmap unflattenGrid . traverse f . flattenGrid

flattenGrid :: (KnownNat n, KnownNat m) => Grid n m a -> Vec (n * m * m * n) a
flattenGrid = concat . gridToRows

unflattenGrid :: (KnownNat n, KnownNat m) => Vec (n * m * m * n) a -> Grid n m a
unflattenGrid = gridFromRows . unconcatI

lastGrid :: forall n m a. (KnownNat n, KnownNat m, 1 <= n * m * m * n) => Grid n m a -> a
lastGrid = last @(n * m * m * n - 1) . flattenGrid

gridToRows :: (KnownNat n, KnownNat m) => Grid n m a -> Vec (n * m) (Vec (m * n) a)
gridToRows = concatMap (fmap concat . transpose . fmap matrixRows) . matrixRows . getGrid

gridFromRows
    :: (KnownNat n, KnownNat m)
    => Vec (n * m) (Vec (m * n) a)
    -> Grid n m a
gridFromRows = Grid . FromRows . fmap (fmap FromRows . transpose) . unconcatI . fmap unconcatI

rowwise
    :: (KnownNat n, KnownNat m)
    => (Vec (n * m) a -> Vec (n * m) b)
    -> Grid n m a
    -> Grid n m b
rowwise f = gridFromRows . fmap f . gridToRows

transposeGrid
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Grid m n a
transposeGrid = gridFromRows . transpose . gridToRows

columnwise
    :: (KnownNat n, KnownNat m)
    => (Vec (n * m) a -> Vec (n * m) b)
    -> Grid n m a
    -> Grid n m b
columnwise f = transposeGrid . rowwise f . transposeGrid

boxwise
    :: (KnownNat n, KnownNat m)
    => (Vec (n * m) a -> Vec (n * m) b)
    -> Grid n m a
    -> Grid n m b
boxwise f = Grid . fmap (fromRowMajorOrder . f . toRowMajorOrder) . getGrid

neighbourhoodwise
    :: (KnownNat n, KnownNat m, Semigroup b)
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Grid n m b
neighbourhoodwise f g = rowwise (repeat . f) g .<>. columnwise (repeat . f) g .<>. boxwise (repeat . f) g
  where
    infixr 6 .<>.
    (.<>.) :: forall f m. (Applicative f, Semigroup m) => f m -> f m -> f m
    (.<>.) = liftA2 (<>)
