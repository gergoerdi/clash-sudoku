{-# LANGUAGE UndecidableInstances #-}
module Sudoku.Board where

import Clash.Prelude
import Sudoku.Matrix

import Clash.Sized.Vector (unsafeFromList)
import Data.Char (ord, chr)

type Square n = BitVector n

wild :: (KnownNat n) => Square n
wild = maxBound

oneHot :: (KnownNat n) => Index n -> Square n
oneHot n = 1 `shiftL` fromIntegral n

data Unique a
    = Unique a
    | Conflict
    | Unset
    deriving (Show)

getUnique :: (KnownNat n) => BitVector (n + 1) -> Unique (Index (n + 1))
getUnique = fold propagate . zipWith start (reverse indicesI) . bitCoerce
  where
    start i True = Unique i
    start i False = Unset

    propagate Unset    y        = y
    propagate x        Unset    = x
    propagate _        _ = Conflict

isUnique :: (KnownNat n) => BitVector (n + 1) -> Bool
isUnique x = case getUnique x of
  Unique{} -> True
  _ -> False

ascii :: Char -> Unsigned 8
ascii = fromIntegral . ord

showSquare :: (KnownNat n, n <= 8) => Square (n + 1) -> Unsigned 8
showSquare x = case getUnique x of
    _ | x == maxBound -> ascii '_'
    Unset -> ascii '?'
    Unique x -> ascii '0' + 1 + fromIntegral x
    Conflict -> ascii 'x'

parseSquare :: (KnownNat n, n <= 8) => Unsigned 8 -> Square (n + 1)
parseSquare x
  | x == ascii '0' = wild
  | x == ascii '_' = wild
  | otherwise = oneHot $ fromIntegral $ x - ascii '0' - 1

newtype Sudoku n m = Sudoku{ getSudoku :: Matrix (n * m) (m * n) (Square (n * m)) }
  deriving (Generic, NFDataX)

showBoard'
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Sudoku n m -> Matrix (n * m) (m * n) Char
showBoard' = map (map (chr . fromIntegral . showSquare)) . getSudoku

instance (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8) => Show (Sudoku n m) where
    show = showBoard

parseBoard
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Vec ((n * m) * (m * n)) (Square (n * m))
    -> Sudoku n m
parseBoard = Sudoku . unconcatI

showBoard
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Sudoku n m -> String
showBoard = unlines . fmap toList . toList . showBoard'

readBoard
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => String -> Sudoku n m
readBoard = parseBoard . map (parseSquare . fromIntegral . ord) . unsafeFromList
