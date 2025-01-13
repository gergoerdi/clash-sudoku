{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wunused-binds #-}
module Sudoku.Pure6 (nice, solve4) where

import Prelude
import Data.List

import Sudoku.Pure.Utils

import qualified Sudoku.Grid as Grid
import Sudoku.Cell
import Data.Char (ord)
import Data.Functor.Compose
import Data.Maybe

fromGrid' :: Grid.Grid 3 3 (Cell 3 3) -> Board
fromGrid' = (getCompose . fmap showCell . Compose) . fromGrid

toGrid' :: Board -> Grid.Grid 3 3 (Cell 3 3)
toGrid' = toGrid . getCompose . fmap toCell . Compose
  where
    toCell = fromMaybe (error "toCell") . parseCell . fromIntegral . ord

type Grid  = Matrix Value

type Row a = [a]

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

rows :: Matrix a -> [Row a]
rows = id

cols :: Matrix a -> [Row a]
cols = rows . transpose

boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack
  where
    pack = split . map split
    split  = chop boxsize
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
choices = getCompose . fmap choice . Compose
  where
    choice v = if empty v then values else [v]

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . map reduce . f

reduce :: Row Choices -> Row Choices
reduce xss =  [xs `minus` singles | xs <- xss]
  where
    singles = concat (filter single xss)

minus :: Choices -> Choices -> Choices
xs `minus` ys =  if single xs then xs else xs \\ ys

complete :: Matrix Choices -> Maybe (Matrix Char)
complete = fmap getCompose . traverse (\xs -> do [x] <- pure xs; pure x) . Compose

void :: Matrix Choices -> Bool
void =  any (any null)

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

expand :: Matrix Choices -> [Matrix Choices]
expand m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
  where
    (rows1,row:rows2) = break (any (not . single)) (rows m)
    (row1,cs:row2)    = break (not . single) row

nice :: (Board -> [Board]) -> Grid.Grid 3 3 (Cell 3 3) -> [Grid.Grid 3 3 (Cell 3 3)]
nice f = fmap toGrid' . f . fromGrid'
