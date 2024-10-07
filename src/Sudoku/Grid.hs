{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fconstraint-solver-iterations=5 #-}
module Sudoku.Grid where

import Clash.Prelude
import Data.Functor.Compose

import Sudoku.Matrix

newtype Grid n m a = Grid{ getGrid :: Matrix n m (Matrix m n a) }
    deriving stock (Generic)
    deriving newtype (NFDataX, BitPack)
    deriving (Functor, Applicative, Foldable) via Compose (Matrix n m) (Matrix m n)

instance (KnownNat n, KnownNat m) => Bundle (Grid n m a) where
    type Unbundled dom (Grid n m a) = Grid n m (Signal dom a)

    bundle = fmap Grid . bundle . fmap bundle . getGrid
    unbundle = Grid . fmap unbundle . unbundle . fmap getGrid

flattenGrid :: Grid n m a -> Vec (n * m * m * n) a
flattenGrid = concat . toRowMajorOrder . fmap toRowMajorOrder . getGrid

unflattenGrid :: (KnownNat n, KnownNat m) => Vec (n * m * m * n) a -> Grid n m a
unflattenGrid = Grid . fmap fromRowMajorOrder . fromRowMajorOrder . unconcatI

headGrid :: forall n m a. (KnownNat n, KnownNat m, 1 <= n * m * m * n) => Grid n m a -> a
headGrid = head @(n * m * m * n - 1) . flattenGrid

gridToRows
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Vec (n * m) (Vec (m * n) a)
gridToRows = concatMap (fmap toRowMajorOrder) . matrixRows . getGrid

gridFromRows
    :: (KnownNat n, KnownNat m)
    => Vec (n * m) (Vec (m * n) a)
    -> Grid n m a
gridFromRows = Grid . FromRows . unconcatI . fmap (FromRows . unconcatI)

rowwise
    :: (KnownNat n, KnownNat m, 1 <= (n * m))
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Grid n m b
rowwise f = gridFromRows . fmap (repeat . f) . gridToRows

transposeGrid
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Grid m n a
transposeGrid = gridFromRows . transpose . gridToRows

colwise
    :: (KnownNat n, KnownNat m, 1 <= (n * m))
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Grid n m b
colwise f = transposeGrid . rowwise f . transposeGrid

toBoxes
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Matrix n m (Vec (n * m) a)
toBoxes = FromRows . fmap (fmap concat . transpose . fmap matrixRows) . matrixRows . getGrid

fromBoxes
    :: forall n m a. (KnownNat n, KnownNat m)
    => Matrix n m (Vec (n * m) a)
    -> Grid n m a
fromBoxes = Grid . FromRows . fmap (fmap FromRows . transpose . fmap unconcatI) . matrixRows

boxwise
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Grid n m b
boxwise f = fromBoxes . fmap (repeat . f) . toBoxes

neighbourhoodwise :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, Semigroup b) => (Vec (n * m) a -> b) -> Grid n m a -> Grid n m b
neighbourhoodwise f g = rowwise f g .<>. colwise f g .<>. boxwise f g
  where
    (.<>.) :: forall f m. (Applicative f, Semigroup m) => f m -> f m -> f m
    (.<>.) = liftA2 (<>)
