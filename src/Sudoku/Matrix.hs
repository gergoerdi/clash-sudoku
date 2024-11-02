{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
module Sudoku.Matrix where

import Clash.Prelude hiding ((.))
import Data.Functor.Compose
import Data.Monoid (Ap(..))
import Data.Coerce
import Data.Isomorphism
import Data.Groupoid
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

isoCoerce :: (Coercible a b) => Iso (->) a b
isoCoerce = Iso coerce coerce

matrix :: (KnownNat n, KnownNat m) => Iso (->) (Matrix n m a) (Vec n (Vec m a))
matrix = isoCoerce

isoConcat :: (KnownNat n, KnownNat m) => Iso (->) (Vec n (Vec m a)) (Vec (n * m) a)
isoConcat = Iso concat unconcatI

isoTranspose :: (KnownNat n, KnownNat m) => Iso (->) (Vec n (Vec m a)) (Vec m (Vec n a))
isoTranspose = Iso transpose transpose

rowMajorOrder :: (KnownNat n, KnownNat m) => Iso (->) (Matrix n m a) (Vec (n * m) a)
rowMajorOrder = isoConcat . matrix

transposeMatrix :: (KnownNat n, KnownNat m) => Iso (->) (Matrix n m a) (Matrix m n a)
transposeMatrix = inv matrix . isoTranspose . matrix
