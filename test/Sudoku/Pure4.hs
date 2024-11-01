{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure4 where

-- import Clash.Prelude hiding (fold, concatMap, toList, minimum, length)
import Prelude

import Sudoku.Solve (Solvable)
import Sudoku.Cell

import Data.Bits
import Data.Monoid.Action
import Data.Foldable (fold, toList)

import Data.Functor.Compose
import Sudoku.Pure.Utils

boxsize = 3

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

type Choices = Cell 3 3

mcp :: Matrix [a] -> [Matrix a]
mcp = fmap getCompose . sequenceA . Compose

-- expand :: forall n m. (Solvable n m) => (Cell n m -> [Cell n m]) -> Matrix (Cell n m) -> [Matrix (Cell n m)]
-- expand possibilities = fmap getCompose . sequenceA . fmap possibilities . Compose
-- expand cm = [rows1 ++ [row1 ++ [c] : row2] ++ rows3 | c ← cs]
-- where (rows1,row : rows2) = break (any best) cm
-- (row1, cs : row2) = break best row
-- best cs = (length cs = n)
-- n = minchoice cm

expandSmallest :: forall n m. (Cell n m -> [Cell n m]) -> Matrix (Cell n m) -> [Matrix (Cell n m)]
expandSmallest possibilities grid = [rows1 ++ [row1 ++ [c] ++ row3] ++ rows3 | c <- cells]
  where
    rows1, rows3 :: [[Cell n m]]
    row1, row3 :: [Cell n m]
    ((fmap (fmap fst) -> rows1), row : (fmap (fmap fst) -> rows3)) = break (any best) grid'
    ((fmap fst -> row1), (_, cells) : (fmap fst -> row3)) = break best row

    best (_, cells) = length cells == n
    n = minchoice grid'

    grid' = getCompose . fmap (\x -> (x, possibilities x)) . Compose $ grid

    minchoice :: Matrix (Cell n m, [Cell n m]) -> Int
    minchoice = minimum . filter (> 1) . toList . fmap (length . snd) . Compose

expandFirst :: forall n m. (Cell n m -> [Cell n m]) -> Matrix (Cell n m) -> [Matrix (Cell n m)]
expandFirst possibilities grid = [rows1 ++ [row1 ++ [c] ++ row3] ++ rows3 | c <- cells]
  where
    ((fmap (fmap fst) -> rows1), row : (fmap (fmap fst) -> rows3)) = break (any best) grid'
    ((fmap fst -> row1), (_, cells) : (fmap fst -> row3)) = break best row

    best = not . single . snd
    grid' = getCompose . fmap (\x -> (x, possibilities x)) . Compose $ grid

single :: [a] -> Bool
single = \case
    [_] -> True
    _ -> False

blocked :: Matrix Choices -> Bool
blocked grid = any (any (== conflicted)) grid || not (safe grid)

safe :: Matrix Choices -> Bool
safe grid = and
    [ all (noDups . filter isUnique) (rows grid)
    , all (noDups . filter isUnique) (cols grid)
    , all (noDups . filter isUnique) (boxs grid)
    ]

search :: Matrix Choices -> [Matrix Choices]
search grid
    | blocked grid             = []
    | all (all isUnique) grid  = [grid]
    | otherwise                = concatMap (search . prune) . expandFirst possibilities $ grid

sudoku :: Matrix Choices -> [Matrix Choices]
sudoku = search . prune

isUnique :: (Solvable n m) => Cell n m -> Bool
isUnique cell = popCount (cellBits cell) == 1

possibilities :: (Solvable n m) => Cell n m -> [Cell n m]
possibilities cell =
    [ cell'
    | i <- [minBound..maxBound]
    , let cell' = unique i
    , cellBits cell .&. cellBits cell' /= 0
    ]

possibilities1 :: (Solvable n m) => Cell n m -> [Cell n m]
possibilities1 cell
    | cont == conflicted
    = [guess]

    | otherwise
    = [guess, cont]
  where
    (guess, cont) = splitCell cell

correct :: forall n m. (Solvable n m) => Matrix (Cell n m) -> Bool
correct b = and
    [ all noDups (rows b)
    , all noDups (cols b)
    , all noDups (boxs b)
    ]

-- noDups :: (Solvable n m, Foldable f) => f (Cell n m) -> Bool
-- noDups xs = all (`elem` xs) (unique <$> [minBound..maxBound])

noDups :: (Solvable n m) => [Cell n m] -> Bool
noDups [] = True
noDups (x:xs) = x `notElem` xs && noDups xs

prune :: (Solvable n m) => Matrix (Cell n m) -> Matrix (Cell n m)
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . fmap reduce . f

    reduce :: (Solvable n m) => [Cell n m] -> [Cell n m]
    reduce cells = fmap (apply (fixed cells)) cells

    fixed :: (Functor f, Foldable f, Solvable n m) => f (Cell n m) -> Mask n m
    fixed = fold . fmap (\x -> if isUnique x then cellMask x else mempty)

    apply mask cell = if isUnique cell then cell else act mask cell
