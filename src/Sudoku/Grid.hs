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

imap :: (Functor f) => Iso (->) a b -> Iso (->) (f a) (f b)
imap iso = Iso (fmap $ embed iso) (fmap $ project iso)

grid :: Iso (->) (Grid n m a) (Matrix n m (Matrix m n a))
grid = Iso coerce coerce

transposeGrid :: (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Grid m n a)
transposeGrid = inv grid . imap transposeMatrix . transposeMatrix . grid

rows :: forall n m a. (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Vec (n * m) (Vec (m * n) a))
rows = imap isoConcat . isoConcat . imap (Iso transpose transpose) . matrix . imap matrix . grid

cols :: forall n m a. (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Vec (n * m) (Vec (m * n) a))
cols = rows . transposeGrid

boxs :: forall n m a. (KnownNat n, KnownNat m) => Iso (->) (Grid n m a) (Vec (n * m) (Vec (m * n) a))
boxs = rowMajorOrder . imap rowMajorOrder . grid

foldNeighbourhoods :: (KnownNat n, KnownNat m, Monoid a) => Grid n m a -> Grid n m a
foldNeighbourhoods = foldBy rows <> foldBy cols <> foldBy boxs
  where
    foldBy
        :: (KnownNat n, KnownNat m, Monoid a)
        => (forall a. Iso (->) (Grid n m a) (Vec (n * m) (Vec (m * n) a)))
        -> Grid n m a
        -> Grid n m a
    foldBy neighbourhood = project neighbourhood . fmap (repeat . fold) . embed neighbourhood

allNeighbourhoods :: (KnownNat n, KnownNat m) => (Vec (n * m) a -> Bool) -> Grid n m a -> Bool
allNeighbourhoods p grid = allBy rows && allBy cols && allBy boxs
  where
    allBy neighbourhood = all p $ embed neighbourhood grid
