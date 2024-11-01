{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
module Sudoku.Matrix where

import Clash.Prelude hiding ((.))
import Data.Functor.Compose
import Data.Monoid (Ap(..))
import Data.Coerce
import Data.Isomorphism
import Control.Category

newtype Matrix n m a = FromRows{ matrixRows :: Vec n (Vec m a) }
    deriving stock (Generic)
    deriving anyclass (NFDataX, BitPack)
    deriving newtype (Eq)
    deriving (Functor, Applicative, Foldable) via Compose (Vec n) (Vec m)
    deriving (Semigroup, Monoid) via Ap (Matrix n m) a

instance (KnownNat n, KnownNat m) => Bundle (Matrix n m a) where
    type Unbundled dom (Matrix n m a) = Matrix n m (Signal dom a)

    bundle = fmap FromRows . bundle . fmap bundle . matrixRows
    unbundle = FromRows . fmap unbundle . unbundle . fmap matrixRows

instance (KnownNat n, KnownNat m) => Traversable (Matrix n m) where
    traverse f = fmap coerce . traverse @(Compose (Vec n) (Vec m)) f . coerce

isoConcat :: (KnownNat n, KnownNat m) => Iso (->) (Vec n (Vec m a)) (Vec (n * m) a)
isoConcat = Iso concat unconcatI

rowMajorOrder :: (KnownNat n, KnownNat m) => Iso (->) (Matrix n m a) (Vec (n * m) a)
rowMajorOrder = isoConcat . Iso coerce coerce

transposeMatrix :: (KnownNat n, KnownNat m) => Iso (->) (Matrix n m a) (Matrix m n a)
transposeMatrix = Iso (FromRows . transpose . matrixRows) (FromRows . transpose . matrixRows)
