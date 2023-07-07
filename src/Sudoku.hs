{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Sudoku where

import Clash.Prelude
import Data.Bits
import Data.Char (isDigit)
-- import qualified Data.List as L
import Clash.Sized.Vector (unsafeFromList)

type Matrix n m a = Vec n (Vec m a)
type Square = BitVector
type Area n m a = Matrix n m (Matrix m n a)
type Sudoku n m = Area n m (Square (n * m))
type Addr n m = (Index n, Index m, Index m, Index n)
type Neighbours n m = Vec (3 * ((n * m) - 1)) (Addr n m)
type Adj n m = Area n m (Neighbours n m)
type Sudoku' = Sudoku 3 3

wild :: (KnownNat n) => BitVector n
wild = 0

oneHot :: (KnownNat n) => Index n -> BitVector n
oneHot n = 1 `shiftL` fromIntegral n

data Unique a
    = Unique a
    | Conflict
    | Unset
    deriving (Show)

getUnique :: (KnownNat n) => Square (n + 1) -> Unique (Index (n + 1))
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
    => Sudoku n m -> Matrix (n * m) (n * m) Char
showBoard' = concatMap \rowBlocks -> map (\i -> map showSquare $ concatMap (!! i) rowBlocks) indicesI

showBoard
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Sudoku n m -> String
showBoard = unlines . fmap toList . toList . showBoard'

readBoard'
    :: forall n m k. (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Vec ((n * m) * (n * m)) (Square (n * m))
    -> Sudoku n m
readBoard' s =
  map (transpose . map ({-map (map readSquare) . -} unconcatI @m @n)) lines
  where
    lines = unconcatI @n @m . unconcatI @(n * m) @(n * m) $ s

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

data Propagate a
    = Unsolvable
    | Underspecified
    | Progress a

adj
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1))
    => Adj n m
adj =
    flip map indicesI \x ->
    flip map indicesI \y ->
    flip map indicesI \z ->
    flip map indicesI \q -> concat $
      unsafeFromList [(x, y, z', q') | z' <- toList indicesI, q' <- toList indicesI, (z', q') /= (z, q)] :>
      unsafeFromList [(x, y', z, q') | y' <- toList indicesI, q' <- toList indicesI, (y', q') /= (y, q)] :>
      unsafeFromList [(x', y, z', q) | x' <- toList indicesI, z' <- toList indicesI, (x', z') /= (x, z)] :>
      Nil


-- propagate1
--     :: forall n m k. (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1))
--     => Sudoku n m -> _ -- Propagate (Sudoku n m)
-- propagate1 = zipWith (zipWith (zipWith (zipWith _))) adj
