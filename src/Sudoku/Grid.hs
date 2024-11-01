{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -fconstraint-solver-iterations=5 #-}
module Sudoku.Grid where

import Clash.Prelude hiding ((.), imap, fold)
import Data.Functor.Compose
import Data.Monoid (Ap(..))
import Data.Coerce
import Data.Isomorphism
import Data.Groupoid
import Control.Category ((.))
import Data.Foldable (fold)

import Sudoku.Matrix

newtype Grid n m a = Grid{ getGrid :: Matrix n m (Matrix m n a) }
    deriving stock (Generic)
    deriving anyclass (NFDataX, BitPack)
    deriving newtype (Eq)
    deriving (Functor, Applicative, Foldable) via Compose (Matrix n m) (Matrix m n)
    deriving (Semigroup, Monoid) via Ap (Grid n m) a

instance (KnownNat n, KnownNat m) => Bundle (Grid n m a) where
    type Unbundled dom (Grid n m a) = Grid n m (Signal dom a)

    bundle = fmap Grid . bundle . fmap bundle . getGrid
    unbundle = Grid . fmap unbundle . unbundle . fmap getGrid

instance (KnownNat n, KnownNat m) => Traversable (Grid n m) where
    traverse f = fmap (project flatGrid) . traverse f . embed flatGrid

flatGrid :: (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Vec (n * m * m * n) a)
flatGrid = isoConcat . rows

lastGrid :: forall n m a. (KnownNat n, KnownNat m, 1 <= n * m * m * n) => Grid n m a -> a
lastGrid = last @(n * m * m * n - 1) . embed flatGrid

foo :: (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Vec n (Vec m (Vec m (Vec n a))))
foo = Iso coerce coerce

imap :: (Functor f) => Iso (->) a b -> Iso (->) (f a) (f b)
imap iso = Iso (fmap $ embed iso) (fmap $ project iso)

transposeGrid'
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Grid m n a
transposeGrid' = Grid . fmap (embed transposeMatrix) . embed transposeMatrix . getGrid

grid :: Iso (->) (Grid n m a) (Matrix n m (Matrix m n a))
grid = Iso coerce coerce

transposeGrid :: (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Grid m n a)
transposeGrid = inv grid . imap transposeMatrix . transposeMatrix . grid

rows :: forall n m a. (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Vec (n * m) (Vec (m * n) a))
rows = isoConcat . imap (imap isoConcat . Iso transpose transpose) . foo

cols :: forall n m a. (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Vec (n * m) (Vec (m * n) a))
cols = rows . transposeGrid

boxs :: forall n m a. (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Vec (n * m) (Vec (m * n) a))
boxs = rowMajorOrder . imap rowMajorOrder . grid

mapBy
    :: (KnownNat n, KnownNat m)
    => (forall a. Iso (->) (Grid n m a) (Vec (n * m) (Vec (m * n) a)))
    -> (Vec (n * m) a -> Vec (n * m) b)
    -> Grid n m a
    -> Grid n m b
mapBy neighbourhood f = project neighbourhood . fmap f . embed neighbourhood

foldBy
    :: (KnownNat n, KnownNat m, Monoid a)
    => (forall a. Iso (->) (Grid n m a) (Vec (n * m) (Vec (m * n) a)))
    -> Grid n m a
    -> Grid n m a
foldBy neighbourhood = project neighbourhood . fmap (repeat . fold) . embed neighbourhood

rowwise
    :: (KnownNat n, KnownNat m)
    => (Vec (n * m) a -> Vec (n * m) b)
    -> Grid n m a
    -> Grid n m b
rowwise = mapBy rows

columnwise
    :: (KnownNat n, KnownNat m)
    => (Vec (n * m) a -> Vec (n * m) b)
    -> Grid n m a
    -> Grid n m b
columnwise = mapBy cols

boxwise
    :: (KnownNat n, KnownNat m)
    => (Vec (n * m) a -> Vec (n * m) b)
    -> Grid n m a
    -> Grid n m b
boxwise = mapBy boxs

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





flattenGrid :: (KnownNat n, KnownNat m) => Grid n m a -> Vec (n * m * m * n) a
flattenGrid = embed flatGrid

unflattenGrid :: (KnownNat n, KnownNat m) => Vec (n * m * m * n) a -> Grid n m a
unflattenGrid = project flatGrid

gridToRows :: (KnownNat n, KnownNat m) => Grid n m a -> Vec (n * m) (Vec (m * n) a)
-- gridToRows = concat . fmap (fmap concat . transpose . fmap matrixRows) . matrixRows . getGrid
-- gridToRows = concat . fmap (fmap concat . transpose) . coerce
-- gridToRows = concat . fmap (_ . fmap matrixRows) . matrixRows . getGrid
gridToRows = embed rows

gridFromRows :: (KnownNat n, KnownNat m) => Vec (n * m) (Vec (m * n) a) -> Grid n m a
gridFromRows = project rows
