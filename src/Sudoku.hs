{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
module Sudoku where

import Clash.Prelude
import Data.Bits
import Data.Char (isDigit)
import Clash.Sized.Vector (unsafeFromList)
import Control.Monad (guard)
import Control.Monad.Writer

type Matrix n m a = Vec n (Vec m a)
type Square n = BitVector n
newtype Sudoku n m = Sudoku{ getSudoku :: Matrix (n * m) (m * n) (Square (n * m)) }

wild :: (KnownNat n) => BitVector n
wild = maxBound

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
    _ | x == maxBound -> '_'
    Unset -> '?'
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

instance (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8) => Show (Sudoku n m) where
    show = showBoard

readBoard'
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Vec ((n * m) * (n * m)) (Square (n * m))
    -> Sudoku n m
readBoard' = Sudoku . unconcatI

readBoard
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => String -> Sudoku n m
readBoard = readBoard' . map readSquare . unsafeFromList

board1 :: Sudoku 3 3
board1 = readBoard . filter isDigit . unlines $
    [ "0 2 0  9 0 8  0 0 0"
    , "8 7 0  0 0 1  0 5 4"
    , "5 0 6  4 0 0  0 1 0"
    , ""
    , "0 0 2  0 0 0  0 9 5"
    , "0 0 0  0 0 0  0 0 0"
    , "9 4 0  0 0 0  8 0 0"
    , ""
    , "0 8 0  0 0 4  5 0 3"
    , "1 3 0  2 0 0  0 8 6"
    , "0 0 0  3 0 7  0 2 0"
    ]

board2 :: Sudoku 3 3
board2 = readBoard . filter isDigit . unlines $
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

others :: (1 <= n) => Vec n a -> Vec n (Vec (n - 1) a)
others (Cons x Nil) = Nil :> Nil
others (Cons x xs@(Cons _ _)) = xs :> map (x :>) (others xs)

simplify :: forall n k k0. (KnownNat n, 1 <= n, KnownNat k, k ~ k0 + 1) => Vec n (Square k) -> WriterT Any Maybe (Vec n (Square k))
simplify xs = traverse (uncurry simplifySquare) (zip xs (others xs))
  where
    simplifySquare :: forall n. Square k -> Vec n (Square k) -> WriterT Any Maybe (Square k)
    simplifySquare x xs = do
        guard $ x' /= 0
        tell $ Any $ x' /= x
        pure x'
      where
        x' = foldl f x xs
        f x y | Unique{} <- getUnique y = x .&. complement y
              | otherwise = x

propagate1
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m -> WriterT Any Maybe (Sudoku n m)
propagate1 s = do
    (s :: Sudoku n m) <- fmap Sudoku . rowwise simplify . getSudoku $ s
    (s :: Sudoku n m) <- fmap Sudoku . columnwise simplify . getSudoku $ s
    (s :: Sudoku n m) <- fmap Sudoku . squarewise @n @m simplify . getSudoku $ s
    return s

propagate
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m -> Maybe (Sudoku n m)
propagate s = do
    (s', Any changed) <- runWriterT $ propagate1 s
    (if changed then propagate else pure) s'
