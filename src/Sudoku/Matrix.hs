{-# LANGUAGE DerivingStrategies, DerivingVia #-}
module Sudoku.Matrix where

import Clash.Prelude
import Data.Functor.Compose

newtype Matrix n m a = FromRows{ matrixRows :: Vec n (Vec m a) }
    deriving stock (Generic)
    deriving anyclass (NFDataX, BitPack)
    deriving (Functor, Applicative, Foldable) via Compose (Vec n) (Vec m)

instance (KnownNat n, KnownNat m) => Bundle (Matrix n m a) where
    type Unbundled dom (Matrix n m a) = Matrix n m (Signal dom a)

    bundle = fmap FromRows . bundle . fmap bundle . matrixRows
    unbundle = FromRows . fmap unbundle . unbundle . fmap matrixRows

toRowMajorOrder :: Matrix n m a -> Vec (n * m) a
toRowMajorOrder = concat . matrixRows

fromRowMajorOrder :: (KnownNat n, KnownNat m) => Vec (n * m) a -> Matrix n m a
fromRowMajorOrder = FromRows . unconcatI

transposeMatrix :: (KnownNat n, KnownNat m) => Matrix n m a -> Matrix m n a
transposeMatrix = FromRows . transpose . matrixRows
