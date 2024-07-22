{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, TupleSections #-}
module Main where

import Clash.Prelude hiding (lift)

import Sudoku.Grid
import Sudoku.Serial (Readable, Writeable, serialIn, serialOut, countSuccChecked)
import Sudoku.Solve
import Sudoku.Stack

import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow
import Data.Char (chr)
import Data.Maybe (catMaybes, maybeToList)
import Control.Monad.Loops

import Debug.Trace
import Text.Printf

import qualified Data.List as L

import Sudoku3 -- (controller, circuit')

readGrid :: (Readable n m) => String -> Maybe (Sudoku n m)
readGrid s = consume input $ simulate @System serialIn input
  where
    input = fmap (Just . ascii) s

    consume :: [Maybe a] -> [Maybe b] -> Maybe b
    consume [] _ = Nothing
    consume (_:xs) (y:ys) = y <|> consume xs ys


-- showGrid :: (Writeable n m) => Sudoku n m -> String
-- showGrid grid = toString . consume $ simulateB @System (serialOut (pure True)) input
--   where
--     input = Just grid : L.repeat Nothing

--     consume ((c, ready):xs) = c : if ready then [] else consume xs
--     toString = fmap (chr . fromIntegral) . catMaybes

-- printGrid :: (Writeable n m) => Sudoku n m -> IO ()
-- printGrid = putStr . showGrid

grid1 :: Sudoku 3 3
Just grid1 = readGrid . unlines $
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

grid2 :: Sudoku 3 3
Just grid2 = readGrid . unlines $
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

mustGuess :: Sudoku 3 3
Just mustGuess = readGrid . unlines $
    [ "8 0 6  4 3 5  0 7 9"
    , "0 3 9  7 0 6  5 4 8"
    , "4 7 5  0 9 8  0 3 6"

    , "0 6 3  8 4 0  7 9 5"
    , "5 4 0  0 7 9  8 6 3"
    , "7 9 8  5 6 3  4 0 0"

    , "6 5 4  3 0 0  9 8 7"
    , "3 0 0  9 8 7  6 5 4"
    , "9 8 7  6 5 4  3 0 0"
    ]

unsolvable :: Sudoku 3 3
Just unsolvable = readGrid . unlines $
    [ "0 0 0  6 0 0  5 0 0"
    , "0 6 0  0 0 8  0 4 0"
    , "0 0 0  7 0 4  0 0 0"
    , ""
    , "0 0 2  8 0 0  0 0 9"
    , "0 9 0  0 0 0  0 7 0"
    , "8 0 0  0 0 9  1 0 0"
    , ""
    , "0 0 0  2 0 6  0 0 0"
    , "0 3 0  5 0 0  0 1 0"
    , "9 5 1  0 0 7  0 0 0"
    ]

-- | From https://norvig.com/sudoku.html
hard :: Sudoku 3 3
Just hard = readGrid . unlines $
    [ ". . . |. . 6 |. . . "
    , ". 5 9 |. . . |. . 8 "
    , "2 . . |. . 8 |. . . "
    , "------+------+------"
    , ". 4 5 |. . . |. . . "
    , ". . 3 |. . . |. . . "
    , ". . 6 |. . 3 |. 5 4 "
    , "------+------+------"
    , ". . . |3 2 5 |. . 6 "
    , ". . . |. . . |. . . "
    , ". . . |. . . |. . . "
    ]

-- solve :: Sudoku 3 3 -> (Maybe (Sudoku 3 3), Int)
-- solve grid = go 0 $ simulateB @System (second bundle . circuit @3 @3) $ mconcat
--     [ [Just grid]
--     , L.replicate 30 Nothing
--     , [Just grid]
--     , L.repeat Nothing
--     ]
--   where
--     go i ((Working', _):xs) = go (i + 1) xs
--     go i ((Unsolvable', _):_) = (Nothing, i)
--     go i ((Solved', grid):_) = (Just grid, i)

-- -- solve' :: Sudoku 3 3 -> Maybe (Sudoku 3 3)
-- solve' grid = Just $ go (Just 0) (pure conflicted) . fmap fst $ simulateB @System (uncurry (circuit'' (SNat @3) (SNat @3))) $ fmap (, True) $ mconcat $
--     [ Just <$> in_bytes
--     , L.repeat Nothing
--     ]
--   where
--     in_bytes = fmap ascii $ showGrid @3 @3 grid

--     go :: Maybe (Index (3 * 3 * 3 * 3)) -> Vec (3 * 3 * 3 * 3) (Cell 3 3) -> [Maybe (Unsigned 8)] -> Sudoku 3 3
--     go Nothing buf _ = unflattenGrid buf
--     -- go (Just i) buf _ | trace (show i) False = undefined
--     go (Just i) buf (out_byte:out_bytes)
--       | Just cell <- {- traceShowId . -} parseCell @3 @3 {- . traceShowId -} =<< out_byte
--       , let (buf', _) = shiftInAtN buf (singleton cell)
--       = go (countSuccChecked i) buf' out_bytes

--       | otherwise
--       = go (Just i) buf out_bytes

-- doSolve :: Sudoku 3 3 -> IO ()
-- doSolve grid = case solve grid of
--     (Nothing, i) -> printf "Unsolvable in %d\n" i
--     (Just grid, i) -> printf "Solved in %d\n%s" i (showGrid grid)

main :: IO ()
main = do
--     -- printGrid grid1
--     doSolve grid1
--     -- printGrid grid2
--     doSolve grid2
--     -- printGrid unsolvable
--     doSolve unsolvable
    return ()
