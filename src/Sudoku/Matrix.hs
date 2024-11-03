{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
module Sudoku.Matrix where

import Clash.Prelude hiding ((.))
import Data.Functor.Compose
import Data.Monoid (Ap(..))
import Sudoku.Iso
import Data.Coerce
import Control.Category ((.))

newtype Matrix n m a = FromRows{ matrixRows :: Vec n (Vec m a) }
    deriving stock (Generic, Eq)
    deriving anyclass (NFDataX)
    deriving (Functor, Applicative, Foldable) via Compose (Vec n) (Vec m)
    deriving (Semigroup, Monoid) via Ap (Matrix n m) a

instance (KnownNat n, KnownNat m) => Bundle (Matrix n m a) where
    type Unbundled dom (Matrix n m a) = Matrix n m (Signal dom a)

    bundle = fmap FromRows . bundle . fmap bundle . matrixRows
    unbundle = FromRows . fmap unbundle . unbundle . fmap matrixRows

instance (KnownNat n, KnownNat m) => Traversable (Matrix n m) where
    traverse f = fmap coerce . traverse @(Compose (Vec n) (Vec m)) f . coerce

matrix :: Matrix n m a <-> Vec n (Vec m a)
matrix = icoerce

iconcat :: (KnownNat n, KnownNat m) => Vec n (Vec m a) <-> Vec (n * m) a
iconcat = Iso concat unconcatI

itranspose :: (KnownNat n, KnownNat m) => Vec n (Vec m a) <-> Vec m (Vec n a)
itranspose = Iso transpose transpose

rowMajorOrder :: (KnownNat n, KnownNat m) => Matrix n m a <-> Vec (n * m) a
rowMajorOrder = iconcat . matrix

transposeMatrix :: (KnownNat n, KnownNat m) => Matrix n m a <-> Matrix m n a
transposeMatrix = inv matrix . itranspose . matrix
