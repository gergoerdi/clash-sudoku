{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -Wunused-binds #-}
module Sudoku.Pure6b {-(nice, solve4)-} where

import Prelude
import Data.List
import qualified Sudoku.Pure.Utils as U

import Clash.Prelude (KnownNat)
import qualified Clash.Sized.Vector as V

import qualified Sudoku.Grid as Grid
import Sudoku.Cell
import Data.Char (chr, ord)
import Data.Functor.Compose
import Data.Maybe
import Control.Monad.State.Strict

fromGrid' :: Grid.Grid 3 3 (Cell 3 3) -> Board
fromGrid' = fmap fromCell . Compose . U.fromGrid
  where
    fromCell = chr . fromIntegral . showCell

toGrid' :: Board -> Grid.Grid 3 3 (Cell 3 3)
toGrid' = fmap toCell . U.toGrid . getCompose
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

empty :: Value -> Bool
empty =  (== '_')

single :: [a] -> Bool
single = \case
    [_] -> True
    _ -> False

transposeMatrix :: Matrix a -> Matrix a
transposeMatrix = Compose . transpose . getCompose

maprow, mapcol, mapbox :: (Row a -> Row b) -> Matrix a -> Matrix b
maprow f = Compose . map f . getCompose
mapcol f = Compose . transpose . map f . transpose . getCompose
mapbox f = Compose . boxes . map f . boxes . getCompose
  where
    boxes :: [Row a] -> [Row a]
    boxes = unpack . pack

    pack = map transpose . split . map split
    split = chop boxsize
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
    choice v = if empty v then values else [v]

prune :: Matrix Choices -> Matrix Choices
prune = mapbox reduce . mapcol reduce . maprow reduce

reduce :: Row Choices -> Row Choices
reduce xss =  [xs `minus` singles | xs <- xss]
  where
    singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys =  if single xs then xs else xs \\ ys

complete :: Matrix Choices -> Maybe (Matrix Char)
complete = traverse \case
    [x] -> Just x
    _ -> Nothing

void :: Matrix Choices -> Bool
void =  any null

safe :: Matrix Choices -> Bool
safe cm = and
    [ all consistent (rows cm)
    , all consistent (cols cm)
    , all consistent (boxs cm)
    ]

consistent :: Row Choices -> Bool
consistent =  nodups . concat . filter single

blocked :: Matrix Choices -> Bool
blocked m = void m || not (safe m)

solve4 :: Grid -> [Grid]
solve4 =  search . prune . choices

search :: Matrix Choices -> [Grid]
search m
    | blocked m            =  []
    | Just m' <- complete m = pure m'
    | otherwise            =  (search . prune) =<< expand m

replaceFirst :: (Traversable f) => f (a, Maybe a) -> (f a, Bool)
replaceFirst xs = runState (traverse (state . replace1) xs) False
  where
    replace1 (x, mb_x') guessed_before
        | not guessed_before
        , Just x' <- mb_x'
        = (x', True)

        | otherwise
        = (x, guessed_before)

expand :: Matrix Choices -> [Matrix Choices]
expand = sequenceA . fst . replaceFirst . fmap (\cs -> ([cs], if single cs then Nothing else Just [[c] | c <- cs]))

nice :: (Board -> [Board]) -> Grid.Grid 3 3 (Cell 3 3) -> [Grid.Grid 3 3 (Cell 3 3)]
nice f = fmap toGrid' . f . fromGrid'
