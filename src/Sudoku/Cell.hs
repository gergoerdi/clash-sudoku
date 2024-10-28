{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sudoku.Cell where

import Clash.Prelude
import Format (ascii)

import Data.Word (Word8)
import Data.Monoid.Action

newtype Cell n m = Cell{ cellBits :: BitVector (n * m) }
    deriving stock (Generic)
    deriving anyclass (NFDataX)
    deriving newtype (Eq, Show, Ord)

wild :: (KnownNat n, KnownNat m) => Cell n m
wild = Cell oneBits

conflicted :: (KnownNat n, KnownNat m) => Cell n m
conflicted = Cell zeroBits

unique :: (KnownNat n, KnownNat m) => Index (n * m) -> Cell n m
unique n = Cell (1 `rotateL` fromIntegral n)

uniqueFromIntegral :: forall a n m. (KnownNat n, KnownNat m, Integral a) => a -> Maybe (Cell n m)
uniqueFromIntegral x
    | x <= fromIntegral (maxBound :: Index (n * m))
    = Just $ unique . fromIntegral $ x

    | otherwise
    = Nothing

lastBit :: (KnownNat n) => BitVector n -> BitVector n
lastBit x = x `xor` (x .&. (x - 1))

splitCell :: (KnownNat n, KnownNat m) => Cell n m -> (Cell n m, Cell n m)
splitCell (Cell c) = (Cell last, Cell rest)
  where
    last = lastBit c
    rest = c .&. complement last

type Textual n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, n * m <= (9 + 26))

decodeOneHot :: (KnownNat n, 1 <= n) => BitVector n -> Index n
decodeOneHot = unpack . collapseBits . fmap pack . zipWith mask indicesI . reverse . bv2v
  where
    mask i en = if bitToBool en then i else 0

    collapseBits :: (KnownNat n, KnownNat m) => Vec n (BitVector m) -> BitVector m
    collapseBits = bitCoerce . fmap reduceOr . transpose . fmap bv2v

showCell :: (Textual n m) => Cell n m -> Word8
showCell x
    | x == wild = ascii '_'
    | x == conflicted = ascii '!'
    | other == conflicted = v'
    | otherwise = ascii '?'
  where
    (x', other) = splitCell x
    v = fromIntegral $ decodeOneHot (cellBits x')
    v' = if v < 9 then ascii '1' + v else ascii 'A' + v - 9

parseCell :: forall n m. (Textual n m) => Word8 -> Maybe (Cell n m)
parseCell x
    | x `elem` [ascii '0', ascii '_', ascii '.']
    = Just wild

    | x `elem` [ascii '!']
    = Just conflicted

    | ascii '1' <= x && x <= ascii '9'
    = uniqueFromIntegral $ x - ascii '1' + 0

    | ascii 'a' <= x && x <= ascii 'z'
    = uniqueFromIntegral $ x - ascii 'a' + 9

    | ascii 'A' <= x && x <= ascii 'Z'
    = uniqueFromIntegral $ x - ascii 'A' + 9

    | otherwise
    = Nothing

newtype Mask n m = Mask{ maskBits :: BitVector (n * m) }
    deriving (Semigroup, Monoid) via Ior (BitVector (n * m))

instance (KnownNat n, KnownNat m) => Action (Mask n m) (Cell n m) where
    act (Mask m) (Cell c) = Cell (c .&. complement m)

cellMask :: (KnownNat n, KnownNat m) => Cell n m -> Mask n m
cellMask = Mask . cellBits
