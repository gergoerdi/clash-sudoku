{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wunused-binds #-}
module Sudoku.Pure7 where

import Prelude
import Data.List

import Clash.Prelude (KnownNat, Type)
import qualified Sudoku.Pure.Utils as U

import qualified Sudoku.Grid as Grid
import Sudoku.Cell hiding (wild)
import Sudoku.Solve (Solvable)
import Data.Char (chr, ord)
import Data.Functor.Compose
import Data.Maybe
import Control.Applicative
import Control.Monad.State.Strict

fromGrid :: (KnownNat n, KnownNat m) => Grid.Grid n m a -> Matrix a
fromGrid = Compose . U.fromGrid

fromGrid' :: Grid.Grid 3 3 (Cell 3 3) -> Board
fromGrid' = fmap (undefined . showCell) . fromGrid

toGrid :: Matrix a -> Grid.Grid 3 3 a
toGrid = U.toGrid . getCompose

toGrid' :: Board -> Grid.Grid 3 3 (Cell 3 3)
toGrid' = fmap toCell . toGrid
  where
    toCell = fromMaybe (error "toCell") . parseCell . fromIntegral . ord

type Grid  = Matrix Value
type Board = Grid

type Matrix = Compose [] Row
type Row = []

type Value = Char
boxsize :: Int
boxsize =  3

values :: [Value]
values =  ['1'..'9']

wild :: Value -> Bool
wild =  (== '_')

single :: [a] -> Bool
single = \case
    [_] -> True
    _ -> False

transposeMatrix :: Matrix a -> Matrix a
transposeMatrix = Compose . transpose . getCompose

maprow, mapcol, mapbox :: (Row a -> Row b) -> Matrix a -> Matrix b
maprow f = Compose . fmap f . getCompose
mapcol f = Compose . transpose . fmap f . transpose . getCompose
mapbox f = Compose . boxes . fmap f . boxes . getCompose
  where
    boxes :: [Row a] -> [Row a]
    boxes = unpack . pack

    pack = map transpose . split . map split
    split = chop boxsize
    unpack = map concat . concat

chop :: Int -> [a] -> [[a]]
chop n [] =  []
chop n xs =  take n xs : chop n (drop n xs)

nodups :: Eq a => [a] -> Bool
nodups = \case
    [] -> True
    (x:xs) -> x `notElem` xs && nodups xs

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices = fmap choice
  where
    choice v = if wild v then values else [v]

prune :: (IsCell c) => Matrix c -> Matrix c
prune = mapbox reduce . mapcol reduce . maprow reduce

class (Traversable g, Foldable (Neighbourhood g)) => IsGrid g where
    type Neighbourhood g :: Type -> Type
    rows', cols', boxs' :: g a -> g (Neighbourhood g a)

smear :: [a] -> [[a]]
smear xs = [xs | _ <- xs]

instance IsGrid Matrix where
    type Neighbourhood Matrix = Row

    rows' = Compose . fmap smear . getCompose
    cols' = transposeMatrix . rows' . transposeMatrix

    boxs' = Compose . fmap smear . unpack . map cols . pack
      where
        pack :: Matrix a -> [Matrix [a]]
        pack = map Compose . split . map split . rows

        split :: [a] -> [[a]]
        split  = chop boxsize

        unpack :: [[[[a]]]] -> [[a]]
        unpack = map concat . concat

rows :: Matrix a -> [Row a]
rows = getCompose

cols :: Matrix a -> [Row a]
cols = rows . transposeMatrix

boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack
  where
    pack :: Matrix a -> [Matrix [a]]
    pack = map Compose . split . map split . rows

    split :: [a] -> [[a]]
    split  = chop boxsize

    unpack :: [[[[a]]]] -> [[a]]
    unpack = map concat . concat

-- instance (Solvable n m) => IsGrid (Grid.Grid n m) where
--     type Neighbourhood (Grid.Grid n m) = Vec (n * m)

--     rows = V.toList . Grid.rows

class IsCell c where
    reduce :: (Foldable t, Functor t) => t c -> t c
    guess :: c -> Maybe [c]
    complete :: c -> Maybe Char

instance IsCell Choices where
    reduce xss = fmap (`minus` singles) xss
      where
        singles = foldMap (\xs -> if single xs then xs else []) xss
        xs `minus` ys = if single xs then xs else xs \\ ys

    guess cs = if single cs then Nothing else Just [[c] | c <- cs]

    complete = \case
        [x] -> Just x
        _ -> Nothing

isUnique :: (Solvable n m) => Cell n m -> Bool
isUnique cell = snd (splitCell cell) == conflicted

instance (Solvable n m) => IsCell (Cell n m) where
    reduce cells = apply mask <$> cells
      where
        mask = foldMap (\cell -> cellMask (isUnique cell) cell) cells
        apply mask cell = act mask (isUnique cell) cell

    guess cell = if next_guess == conflicted then Nothing else Just [first_guess, next_guess]
      where
        (first_guess, next_guess) = splitCell cell

    -- complete cell
    --     | isUnique cell = Just $ values !! fromIntegral (decodeOneHot (cellBits cell))
    --     | otherwise = Nothing

void :: Matrix Choices -> Bool
void = any null

safe :: Matrix Choices -> Bool
safe cm = and
    [ all consistent (rows cm)
    , all consistent (cols cm)
    , all consistent (boxs cm)
    ]

consistent :: Row Choices -> Bool
consistent =  nodups . concat . filter single

solve4 :: Grid -> [Grid]
solve4 =  search . prune . choices

search :: (Alternative f) => Matrix Choices -> f Grid
search m
    | not (safe m)                    = empty
    | any null m                      = empty
    | Just m' <- traverse complete m  = pure m'
    | otherwise                       = asum [search . prune $ m' | m' <- expand m]

replaceFirst :: (Traversable t) => t (a, Maybe a) -> (t a, Bool)
replaceFirst xs = runState (traverse (state . replace1) xs) False
  where
    replace1 (x, mb_x') guessed_before
        | not guessed_before
        , Just x' <- mb_x'
        = (x', True)

        | otherwise
        = (x, guessed_before)

expand :: (IsCell c, Traversable t) => t c -> [t c]
expand = sequenceA . fst . replaceFirst . fmap (\cs -> ([cs], guess cs))

nice :: (Board -> [Board]) -> Grid.Grid 3 3 (Cell 3 3) -> [Grid.Grid 3 3 (Cell 3 3)]
nice f = fmap toGrid' . f . fromGrid'
