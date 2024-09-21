{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses #-}
{-# OPTIONS -fconstraint-solver-iterations=5 #-}
{-# LANGUAGE InstanceSigs #-}
module Sudoku.Grid where

import Clash.Prelude
import Data.Functor.Compose
import Data.Coerce (coerce)

import Sudoku.Matrix
import Sudoku.Cell

newtype Grid n m a = Grid{ getGrid :: Matrix n m (Matrix m n a) }
    deriving stock (Generic)
    deriving newtype (NFDataX, BitPack)
    deriving (Functor) via Compose (Matrix n m) (Matrix m n)
    -- deriving (Applicative) via Compose (Vec n) (Vec m)
    deriving (Foldable) via Compose (Matrix n m) (Matrix m n)

instance (KnownNat n, KnownNat m) => Applicative (Grid n m) where
    {-# INLINE pure #-}
    pure :: forall a. a -> Grid n m a
    pure = coerce (pure @(Compose (Matrix n m) (Matrix m n)) @a)

    -- {-# INLINE (<*>) #-}
    Grid gf <*> Grid gx = Grid $ liftA2 (<*>) gf gx

instance (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Traversable (Grid n m) where
    traverse f = fmap Grid . traverse (traverse f) . getGrid

instance (KnownNat n, KnownNat m) => Bundle (Grid n m a) where
    type Unbundled dom (Grid n m a) = Grid n m (Signal dom a)

    bundle = fmap Grid . bundle . fmap bundle . getGrid
    unbundle = Grid . fmap unbundle . unbundle . fmap getGrid

flattenGrid :: Grid n m a -> Vec (n * m * m * n) a
flattenGrid = concat . toRowMajorOrder . fmap toRowMajorOrder . getGrid

unflattenGrid :: (KnownNat n, KnownNat m) => Vec (n * m * m * n) a -> Grid n m a
unflattenGrid = Grid . fmap fromRowMajorOrder . fromRowMajorOrder . unconcatI

type Sudoku n m = Grid n m (Cell n m)

emptySudoku :: (KnownNat n, KnownNat m) => Sudoku n m
emptySudoku = pure conflicted

others :: (1 <= n) => Vec n a -> Vec n (Vec (n - 1) a)
others (Cons x Nil) = Nil :> Nil
others (Cons x xs@(Cons _ _)) = xs :> map (x :>) (others xs)

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

rowmap
    :: (KnownNat n, KnownNat m, 1 <= (n * m))
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Vec (n * m) b
rowmap f = fmap f . gridToRows

transposeGrid
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Grid m n a
transposeGrid = gridFromRows . transpose . gridToRows

colmap
    :: (KnownNat n, KnownNat m, 1 <= (n * m))
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Vec (n * m) b
colmap f = rowmap f . transposeGrid

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

boxmap
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Matrix n m b
boxmap f = fmap f . toBoxes
