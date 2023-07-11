{-# LANGUAGE UndecidableInstances #-}
module Sudoku.Board where

import Clash.Prelude
import qualified Sudoku.Matrix as M

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
isUnique x = popCount x == 1

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

newtype Sudoku n m = Sudoku{ getSudoku :: M.Matrix (n * m) (m * n) (Square (n * m)) }
  deriving (Generic, NFDataX)

parseBoard
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Vec ((n * m) * (m * n)) (Square (n * m))
    -> Sudoku n m
parseBoard = Sudoku . unconcatI

instance (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8) => Show (Sudoku n m) where
    show = unlines . fmap toList . toList . map (map (chr . fromIntegral . showSquare)) . getSudoku

rowwise
    :: (KnownNat n, KnownNat m, 1 <= (m * n), Applicative f)
    => (Vec (m * n) (Square (n * m)) -> f (Vec (m * n) (Square (n * m))))
    -> Sudoku n m
    -> f (Sudoku n m)
rowwise f = fmap Sudoku . M.rowwise f . getSudoku

columnwise
    :: (KnownNat n, KnownNat m, 1 <= (m * n), Applicative f)
    => (Vec (n * m) (Square (n * m)) -> f (Vec (n * m) (Square (n * m))))
    -> Sudoku n m
    -> f (Sudoku n m)
columnwise f = fmap Sudoku . M.columnwise f . getSudoku

squarewise
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall f. (Applicative f)
    => (Vec (n * m) (Square (n * m)) -> f (Vec (n * m) (Square (n * m))))
    -> Sudoku n m
    -> f (Sudoku n m)
squarewise f = fmap Sudoku . M.squarewise @m @n f . getSudoku
