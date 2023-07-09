{-# LANGUAGE AllowAmbiguousTypes #-}
module Sudoku.Matrix where

import Clash.Prelude hiding (lift)
import RetroClash.Utils hiding (oneHot)

import Data.Bits
import Data.Char (isDigit)
import Data.Maybe
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad (guard)
import Control.Monad.Writer
import Control.Monad.State

type Matrix n m a = Vec n (Vec m a)

rowwise
    :: (KnownNat n, KnownNat m, 1 <= n, Applicative f)
    => (Vec m a -> f (Vec m b))
    -> Matrix n m a
    -> f (Matrix n m b)
rowwise f = traverse f

columnwise
    :: (KnownNat n, KnownNat m, 1 <= m, Applicative f)
    => (Vec n a -> f (Vec n b))
    -> Matrix n m a
    -> f (Matrix n m b)
columnwise f = fmap transpose . traverse f . transpose

toSquares
    :: (KnownNat n, KnownNat m)
    => Matrix (n * m) (m * n) a
    -> Matrix n m (Vec (n * m) a)
toSquares = map (map concat . transpose . map unconcatI) . unconcatI

fromSquares
    :: (KnownNat n, KnownNat m)
    => Matrix n m (Vec (n * m) a)
    -> Matrix (n * m) (m * n) a
fromSquares = concat . map (map concat . transpose . map unconcatI)

squarewise
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall f a b. (Applicative f)
    => (Vec (n * m) a -> f (Vec (n * m) b))
    -> Matrix (n * m) (m * n) a
    -> f (Matrix (n * m) (m * n) b)
squarewise f = fmap fromSquares . traverse (traverse f) . toSquares @n @m
