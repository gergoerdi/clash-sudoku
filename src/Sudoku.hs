{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
module Sudoku where

import Clash.Prelude
import Data.Bits
import Data.Char (isDigit)
import Clash.Sized.Vector (unsafeFromList)

type Matrix n m a = Vec n (Vec m a)
type Square n = BitVector n
newtype Sudoku n m = Sudoku{ getSudoku :: Matrix (n * m) (m * n) (Square (n * m)) }

wild :: (KnownNat n) => BitVector n
wild = 0

oneHot :: (KnownNat n) => Index n -> BitVector n
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

showSquare :: (KnownNat n, n <= 8) => Square (n + 1) -> Char
showSquare x = case getUnique x of
    Unset -> '_'
    Unique x -> ('1' :> '2' :> '3' :> '4' :> '5' :> '6' :> '7' :> '8' :> '9' :> Nil) !! x
    Conflict -> 'x'

readSquare :: (KnownNat n, n <= 8) => Char -> Square (n + 1)
readSquare '0' = wild
readSquare '_' = wild
readSquare n = oneHot $ fromInteger (read [n] - 1)

showBoard'
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Sudoku n m -> Matrix (n * m) (m * n) Char
showBoard' = map (map showSquare) . getSudoku

showBoard
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Sudoku n m -> String
showBoard = unlines . fmap toList . toList . showBoard'

readBoard'
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Vec ((n * m) * (n * m)) (Square (n * m))
    -> Sudoku n m
readBoard' = Sudoku . unconcatI

readBoard
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => String -> Sudoku n m
readBoard = readBoard' . map readSquare . unsafeFromList

board :: Sudoku 3 3
board = readBoard . filter isDigit . unlines $
    [ "0 0 0  6 0 0  5 0 0"
    , "0 6 0  0 0 8  0 4 0"
    , "0 0 0  7 0 4  0 0 0"
    , ""
    , "0 0 2  8 0 0  0 0 9"
    , "0 9 0  0 0 0  0 7 0"
    , "8 0 0  0 0 9  1 0 0"
    , ""
    , "0 0 0  2 0 6  0 0 0"
    , "0 0 0  5 0 0  0 1 0"
    , "0 5 1  0 0 7  0 0 0"
    ]

