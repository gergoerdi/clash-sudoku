{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Pure4 where

-- import Clash.Prelude hiding (fold, concatMap, toList, minimum, length)
import Prelude
import Clash.Prelude (KnownNat)
import qualified Clash.Sized.Vector as V

import Sudoku.Solve (Solvable)
import qualified Sudoku.Grid as Grid
import Sudoku.Cell

import Data.Bits
-- import Data.Monoid (All(..))
import Data.Monoid.Action
import Data.Foldable (fold, toList, minimum, length)
import Control.Monad.State.Strict

import Data.Functor.Compose

type Matrix a = [[a]]

boxsize = 3

fromGrid :: (KnownNat n, KnownNat m) => Grid.Grid n m a -> Matrix a
fromGrid = V.toList . fmap V.toList . Grid.gridToRows

toGrid :: Matrix a -> Grid.Grid 3 3 a
toGrid = Grid.gridFromRows . V.unsafeFromList . fmap V.unsafeFromList

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

expand :: forall n m. (Cell n m -> [Cell n m]) -> Matrix (Cell n m) -> [Matrix (Cell n m)]
expand possibilities grid = [rows1 ++ [row1 ++ [c] ++ row3] ++ rows3 | c <- cells]
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

-- expand :: forall n m. (Solvable n m) => (Cell n m -> [Cell n m]) -> Matrix (Cell n m) -> [Matrix (Cell n m)]
-- expand possibilities grid = fmap getCompose . sequenceA $ evalState (traverse (state . guess1) . Compose $ grid) False
--   where
--     guess1 x guessed_before
--         | not guessed_before
--         , xs@(_:_:_) <- possibilities x
--         = (xs, True)

--         | otherwise
--         = ([x], guessed_before)

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
    | otherwise                = concatMap (search . prune) . expand possibilities $ grid

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

-- expandFirst :: forall n m. (Solvable n m) => Sudoku n m -> [Sudoku n m]
-- expandFirst grid = sequenceA $ evalState (traverse (state . guess1) grid) False
--   where
--     guess1 x guessed_before
--         | not guessed_before
--         , xs@(_:_:_) <- possibilities1 x
--         = (xs, True)

--         | otherwise
--         = ([x], guessed_before)

-- expandSmallest :: forall n m. (Solvable n m) => Sudoku n m -> [Sudoku n m]
-- expandSmallest grid = sequenceA $ evalState (traverse (state . guess1) grid') False
--   where
--     grid' = (\x -> (x, possibilities x)) <$> grid
--     n = minimum . filter (> 1) . toList . fmap (length . snd) $ grid'

--     guess1 (x, xs) guessed_before
--         | not guessed_before
--         , length xs == n
--         = (xs, True)

--         | otherwise
--         = ([x], guessed_before)

-- correct :: forall n m. (Solvable n m) => Sudoku n m -> Bool
-- correct = getAll . fold . neighbourhoodwise noDups
--   where
--     noDups :: Vec (n * m) (Cell n m) -> All
--     noDups xs = All $ all (`elem` xs) (unique <$> [minBound..maxBound])


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

-- correct :: (Solvable n m) => Matrix (Cell n m) -> Bool
-- correct = all (all isUnique)

-- search :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
-- search grid
--     | any (== conflicted) grid = []
--     | all isUnique grid       = [grid]
--     | otherwise               = concatMap (search . prune) . expandFirst $ grid

prune :: (Solvable n m) => Matrix (Cell n m) -> Matrix (Cell n m)
prune = pruneBy boxs . pruneBy cols . pruneBy rows
  where
    pruneBy f = f . fmap reduce . f

    reduce :: (Solvable n m) => [Cell n m] -> [Cell n m]
    reduce cells = fmap (apply (fixed cells)) cells

    fixed :: (Functor f, Foldable f, Solvable n m) => f (Cell n m) -> Mask n m
    fixed = fold . fmap (\x -> if isUnique x then cellMask x else mempty)

    apply mask cell = if isUnique cell then cell else act mask cell

-- prune grid =
--     case neighbourhoodMasks masks of
--         Nothing -> pure conflicted
--         Just neighbourhood_masks ->
--           apply <$> uniques <*> neighbourhood_masks <*> grid
--   where
--     uniques = isUnique <$> grid
--     masks = maskOf <$> uniques <*> grid
--     neighbourhood_masks = neighbourhoodwise fold masks

--     maskOf is_unique cell = if is_unique then cellMask cell else mempty
--     apply is_unique mask = if is_unique then id else act mask

-- solve' :: (Solvable n m) => Sudoku n m -> [Sudoku n m]
-- solve' = search . prune
