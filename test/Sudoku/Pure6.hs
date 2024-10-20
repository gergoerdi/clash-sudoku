{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure6 where

import Prelude
import Data.List

import Clash.Prelude (KnownNat)
import qualified Clash.Sized.Vector as V

import qualified Sudoku.Grid as Grid
import Sudoku.Cell
import Data.Char (chr, ord)
import Data.Functor.Compose
import Data.Maybe

fromGrid :: (KnownNat n, KnownNat m) => Grid.Grid n m a -> Matrix a
fromGrid = V.toList . fmap V.toList . Grid.gridToRows

fromGrid' :: Grid.Grid 3 3 (Cell 3 3) -> Board
fromGrid' = (getCompose . fmap fromCell . Compose) . fromGrid
  where
    fromCell = chr . fromIntegral . showCell

toGrid :: Matrix a -> Grid.Grid 3 3 a
toGrid = Grid.gridFromRows . V.unsafeFromList . fmap V.unsafeFromList

toGrid' :: Board -> Grid.Grid 3 3 (Cell 3 3)
toGrid' = toGrid . getCompose . fmap toCell . Compose
  where
    toCell = fromMaybe (error "toCell") . parseCell . fromIntegral . ord


type Grid             = Matrix Value
type Board = Grid

type Matrix a         = [Row a]

type Row a            = [a]

type Value            = Char
boxsize               :: Int
boxsize               =  3

values                :: [Value]
values                =  ['1'..'9']

empty                 :: Value -> Bool
empty                 =  (== '_')

single                :: [a] -> Bool
single [_]            =  True
single _              =  False
easy                  :: Grid
easy                  =  ["2____1_38",
                          "________5",
                          "_7___6___",
                          "_______13",
                          "_981__257",
                          "31____8__",
                          "9__8___2_",
                          "_5__69784",
                          "4__25____"]
gentle                :: Grid
gentle                =  ["_1_42___5",
                          "__2_71_39",
                          "_______4_",
                          "2_71____6",
                          "____4____",
                          "6____74_3",
                          "_7_______",
                          "12_73_5__",
                          "3___82_7_"]
diabolical            :: Grid
diabolical            =  ["_9_7__86_",
                          "_31__5_2_",
                          "8_6______",
                          "__7_5___6",
                          "___3_7___",
                          "5___1_7__",
                          "______1_9",
                          "_2_6__35_",
                          "_54__8_7_"]
unsolvable            :: Grid
unsolvable            =  ["1__9_7__3",
                          "_8_____7_",
                          "__9___6__",
                          "__72_94__",
                          "41_____95",
                          "__85_43__",
                          "__3___7__",
                          "_5_____4_",
                          "2__8_6__9"]
minimal               :: Grid
minimal               =  ["_98______",
                          "____7____",
                          "____15___",
                          "1________",
                          "___2____9",
                          "___9_6_82",
                          "_______3_",
                          "5_1______",
                          "___4___2_"]
blank                 :: Grid
blank                 =  replicate n (replicate n '_')
                         where n = boxsize ^ 2
rows                  :: Matrix a -> [Row a]
rows                  =  id
cols                  :: Matrix a -> [Row a]
cols                  =  transpose
boxs                  :: Matrix a -> [Row a]
boxs                  =  unpack . map cols . pack
                         where
                            pack   = split . map split
                            split  = chop boxsize
                            unpack = map concat . concat

chop                  :: Int -> [a] -> [[a]]
chop n []             =  []
chop n xs             =  take n xs : chop n (drop n xs)
valid                 :: Grid -> Bool
valid g               =  all nodups (rows g) &&
                         all nodups (cols g) &&
                         all nodups (boxs g)

nodups                :: Eq a => [a] -> Bool
nodups []             =  True
nodups (x:xs)         =  not (elem x xs) && nodups xs
type Choices          =  [Value]

choices               :: Grid -> Matrix Choices
choices               =  map (map choice)
                         where
                            choice v = if empty v then values else [v]
cp                    :: [[a]] -> [[a]]
cp []                 =  [[]]
cp (xs:xss)           =  [y:ys | y <- xs, ys <- cp xss]
collapse              :: Matrix [a] -> [Matrix a]
collapse              =  cp . map cp
solve                 :: Grid -> [Grid]
solve                 =  filter valid . collapse . choices
prune                 :: Matrix Choices -> Matrix Choices
prune                 =  pruneBy boxs . pruneBy cols . pruneBy rows
                         where pruneBy f = f . map reduce . f

reduce                :: Row Choices -> Row Choices
reduce xss            =  [xs `minus` singles | xs <- xss]
                         where singles = concat (filter single xss)

minus                 :: Choices -> Choices -> Choices
xs `minus` ys         =  if single xs then xs else xs \\ ys
solve2                :: Grid -> [Grid]
solve2                =  filter valid . collapse . prune . choices
solve3                :: Grid -> [Grid]
solve3                =  filter valid . collapse . fix prune . choices

fix                   :: Eq a => (a -> a) -> a -> a
fix f x               =  if x == x' then x else fix f x'
                         where x' = f x
complete              :: Matrix Choices -> Bool
complete              =  all (all single)
void                  :: Matrix Choices -> Bool
void                  =  any (any null)
safe                  :: Matrix Choices -> Bool
safe cm               =  all consistent (rows cm) &&
                         all consistent (cols cm) &&
                         all consistent (boxs cm)

consistent            :: Row Choices -> Bool
consistent            =  nodups . concat . filter single
blocked               :: Matrix Choices -> Bool
blocked m             =  void m || not (safe m)
solve4                :: Grid -> [Grid]
solve4                =  search . prune . choices

search                :: Matrix Choices -> [Grid]
search m
 | blocked m          =  []
 | complete m         =  collapse m
 | otherwise          =  [g | m' <- expand m
                            , g  <- search (prune m')]
expand                :: Matrix Choices -> [Matrix Choices]
expand m              =
   [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      (rows1,row:rows2) = break (any (not . single)) m
      (row1,cs:row2)    = break (not . single) row

-- main                  :: IO ()
-- main                  =  putStrLn (unlines (head (solve4 easy)))

nice :: (Board -> [Board]) -> Grid.Grid 3 3 (Cell 3 3) -> [Grid.Grid 3 3 (Cell 3 3)]
nice f = fmap toGrid' . f . fromGrid'
