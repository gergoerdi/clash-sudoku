{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Sudoku.Pure5 where

import Prelude
import qualified Sudoku.Grid as Grid
import Sudoku.Cell
import Data.Char (chr, ord)

import Data.Maybe
import Sudoku.Pure.Utils

import Data.Functor.Compose

boxsize = 3
boardsize = 9
cellvals = "123456789"
blank = (== '_')

fromGrid' :: Grid.Grid 3 3 (Cell 3 3) -> Board
fromGrid' = (getCompose . fmap fromCell . Compose) . fromGrid
  where
    fromCell = chr . fromIntegral . showCell

toGrid' :: Board -> Grid.Grid 3 3 (Cell 3 3)
toGrid' = toGrid . getCompose . fmap toCell . Compose
  where
    toCell = fromMaybe (error "toCell") . parseCell . fromIntegral . ord

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . fmap cols . group . fmap group
  where
    group :: [a] -> [[a]]
    group = groupBy boxsize

    ungroup :: [[a]] -> [a]
    ungroup = concat

    groupBy :: Int -> [a] -> [[a]]
    groupBy n [] = []
    groupBy n xs = case splitAt n xs of
        (xs', xss') -> xs' : groupBy n xss'

type Choices = [Char]

choices :: Board -> Matrix Choices
choices = getCompose . fmap parse . Compose
  where
    parse e = if blank e then cellvals else [e]

mcp :: Matrix [a] -> [Matrix a]
mcp = fmap getCompose . sequenceA . Compose

expand :: Matrix Choices -> [Matrix Choices]
expand cm = [rows1 ++ [row1 ++ [c] : row3] ++ rows3 | c <- cs]
  where
    (rows1, row : rows3) = break (any best) cm
    (row1, cs : row3) = break best row

    best = not . single

blocked :: Matrix Choices -> Bool
blocked grid = any (any null) grid || not (safe grid)

safe :: Matrix Choices -> Bool
safe grid = and
    [ all (noDups . fixed) (rows grid)
    , all (noDups . fixed) (cols grid)
    , all (noDups . fixed) (boxs grid)
    ]

search :: Matrix Choices -> [Matrix Choices]
search grid
    | blocked grid           = []
    | all (all single) grid  = [grid]
    | otherwise              = concatMap (search . prune) . expand $ grid

sudoku :: Board -> [Board]
sudoku = fmap (getCompose . fmap head . Compose) . search . prune . choices

sudoku' :: Grid.Grid 3 3 (Cell 3 3) -> [Grid.Grid 3 3 (Cell 3 3)]
sudoku' = fmap toGrid' . sudoku . fromGrid'

correct :: (Eq a) => Matrix a -> Bool
correct b = and
    [ all noDups (rows b)
    , all noDups (cols b)
    , all noDups (boxs b)
    ]

noDups :: (Eq a) => [a] -> Bool
noDups [] = True
noDups (x:xs) = x `notElem` xs && noDups xs

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . fmap reduce . f

    reduce :: [Choices] -> [Choices]
    reduce css = fmap (remove (fixed css)) css

    remove fs cs = if single cs then cs else delete fs cs

    delete fs = filter (`notElem` fs)

fixed :: [Choices] -> Choices
fixed = concat . filter single

single :: [a] -> Bool
single = \case
    [x] -> True
    _ -> False
