{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving, DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
module Sudoku.Matrix where

import Clash.Prelude
import Data.Functor.Compose
import Data.Coerce (coerce)

newtype Matrix n m a = FromRows{ matrixRows :: Vec n (Vec m a) }
    deriving stock (Generic)
    deriving newtype (NFDataX, BitPack)
    deriving (Functor) via Compose (Vec n) (Vec m)
    -- deriving (Applicative) via Compose (Vec n) (Vec m)
    deriving (Foldable) via Compose (Vec n) (Vec m)

instance (KnownNat n, KnownNat m) => Applicative (Matrix n m) where
    {-# INLINE pure #-}
    pure :: forall a. a -> Matrix n m a
    pure = coerce (pure @(Compose (Vec n) (Vec m)) @a)

    {-# INLINE (<*>) #-}
    (<*>) :: forall a b. Matrix n m (a -> b) -> Matrix n m a -> Matrix n m b
    mf <*> mx = FromRows $ zipWith (<*>) (matrixRows mf) (matrixRows mx)

instance (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Traversable (Matrix n m) where
    traverse f = fmap FromRows . traverse (traverse f) . matrixRows

instance (KnownNat n, KnownNat m) => Bundle (Matrix n m a) where
    type Unbundled dom (Matrix n m a) = Matrix n m (Signal dom a)

    bundle = fmap FromRows . bundle . fmap bundle . matrixRows
    unbundle = FromRows . fmap unbundle . unbundle . fmap matrixRows

generateMatrix :: (KnownNat n, KnownNat m) => (Index n -> Index m -> a) -> Matrix n m a
generateMatrix f = FromRows $
    flip map indicesI \i ->
    flip map indicesI \j ->
    f i j

toRowMajorOrder :: Matrix n m a -> Vec (n * m) a
toRowMajorOrder = concat . matrixRows

fromRowMajorOrder :: (KnownNat n, KnownNat m) => Vec (n * m) a -> Matrix n m a
fromRowMajorOrder = FromRows . unconcatI

transposeMatrix :: (KnownNat n, KnownNat m) => Matrix n m a -> Matrix m n a
transposeMatrix = FromRows . transpose . matrixRows
