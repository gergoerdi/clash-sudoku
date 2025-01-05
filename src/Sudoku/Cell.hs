{-# LANGUAGE DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sudoku.Cell where

import Clash.Prelude
import Clash.Num.Overflowing
import Format (ascii)
import Sudoku.Utils ()
import Data.Char (ord, chr)

import Data.Word (Word8)
import Data.Monoid.Action

newtype Cell n m = Cell{ cellBits :: BitVector (n * m) }
    deriving stock (Generic)
    deriving anyclass (NFDataX)
    deriving newtype (Eq, Show, Ord)

canBe :: (KnownNat n, KnownNat m) => Cell n m -> Index (n * m) -> Bool
Cell c `canBe` i = c ! i == 1

wild :: (KnownNat n, KnownNat m) => Cell n m
wild = Cell oneBits

conflicted :: (KnownNat n, KnownNat m) => Cell n m
conflicted = Cell zeroBits

given :: (KnownNat n, KnownNat m) => Index (n * m) -> Cell n m
given i = Cell (bit $ fromIntegral i)

givenFromIntegral :: forall n m a. (KnownNat n, KnownNat m, Integral a) => a -> Maybe (Cell n m)
givenFromIntegral x
    | x < snatToNum (SNat @(n * m))
    = Just $ given . fromIntegral $ x

    | otherwise
    = Nothing

lastBit :: (KnownNat n) => BitVector n -> BitVector n
lastBit x = x .&. negate x

splitCell :: (KnownNat n, KnownNat m) => Cell n m -> (Cell n m, Cell n m)
splitCell (Cell c) = (Cell last, Cell rest)
  where
    last = lastBit c
    rest = c .&. complement last

decodeOneHot :: forall n. (KnownNat n, 1 <= n) => BitVector n -> Index n
decodeOneHot = fold @(n - 1) (.|.) . zipWith mask (reverse indicesI) . bv2v
  where
    mask i sel = if bitToBool sel then i else 0

type Textual n m = (KnownNat n, KnownNat m, 1 <= n * m, n * m <= (9 + 26))

showCell :: (Textual n m) => Cell n m -> Char
showCell x
    | x == wild = '_'
    | x == conflicted = '!'
    | other == conflicted = v'
    | otherwise = '?'
  where
    (x', other) = splitCell x
    v = fromIntegral $ decodeOneHot (cellBits x') :: Word8
    v' = chr . fromIntegral $ if v < 9 then fromIntegral (ord '1') + v else fromIntegral (ord 'A') + v - 9

parseCell :: forall n m. (Textual n m) => Word8 -> Maybe (Cell n m)
parseCell x
    | x `elem` [ascii '0', ascii '_', ascii '.']
    = Just wild

    | x `elem` [ascii '!']
    = Just conflicted

    | ascii '1' <= x && x <= ascii '9'
    = givenFromIntegral $ x - ascii '1' + 0

    | ascii 'a' <= x && x <= ascii 'z'
    = givenFromIntegral $ x - ascii 'a' + 9

    | ascii 'A' <= x && x <= ascii 'Z'
    = givenFromIntegral $ x - ascii 'A' + 9

    | otherwise
    = Nothing

newtype Mask n m = Mask{ maskBits :: BitVector (n * m) }
    deriving (Semigroup, Monoid) via Ior (BitVector (n * m))

instance (KnownNat n, KnownNat m) => Action (Mask n m) (Cell n m) where
    act (Mask m) (Cell c) = Cell (c .&. complement m)

cellMask :: (KnownNat n, KnownNat m) => Cell n m -> Mask n m
cellMask = Mask . cellBits

bitsOverlap :: (KnownNat n, KnownNat k) => Vec k (BitVector n) -> Bool
bitsOverlap = any (hasOverflowed . sum . map toOverflowing) . transpose . map bv2v
