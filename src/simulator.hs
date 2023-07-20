{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf #-}
module Main where

import Clash.Prelude hiding (lift)

import Sudoku.Grid
import Sudoku.Serial (Readable, Writeable, serialIn, serialOut)
import Sudoku.Solve
import Sudoku.Stack

import Control.Monad.Writer
import Control.Monad.State
import Data.Char (chr)
import Data.Maybe (catMaybes, maybeToList)
import Control.Monad.Loops

import qualified Data.List as L

import Sudoku -- (controller, circuit')

readGrid :: (Readable n m) => String -> Maybe (Sudoku n m)
readGrid s = consume input $ simulate @System serialIn input
  where
    input = fmap (Just . ascii) s

    consume :: [Maybe a] -> [Maybe b] -> Maybe b
    consume [] _ = Nothing
    consume (_:xs) (y:ys) = y <|> consume xs ys


showGrid :: (Writeable n m) => Sudoku n m -> String
showGrid grid = toString . consume $ simulateB @System (serialOut (pure True)) input
  where
    input = Just grid : L.repeat Nothing

    consume ((c, ready):xs) = c : if ready then [] else consume xs
    toString = fmap (chr . fromIntegral) . catMaybes

printGrid :: (Writeable n m) => Sudoku n m -> IO ()
printGrid = putStr . showGrid

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

solve :: Sudoku 3 3 -> Maybe (Sudoku 3 3)
solve grid = go $ simulate @System (circuit @3 @3) $ Just grid : L.repeat Nothing
  where
    go (Working:xs) = go xs
    go (Unsolvable:_) = Nothing
    go (Solution grid:_) = Just grid

doSolve :: Sudoku 3 3 -> IO ()
doSolve grid = case solve grid of
    Nothing -> putStrLn "Unsolvable"
    Just grid -> putStrLn "Solution:" >> printGrid grid

main :: IO ()
main = do
    printGrid grid1
    doSolve grid1
    printGrid grid2
    doSolve grid2
    return ()

foo = fmap niceR $ simulate @System (circuit @3 @3) $
    L.replicate 3 Nothing <> [Just grid1] <> L.repeat Nothing
  where
    niceR Idle = "Idle"
    niceR (Solution grid) = showGrid grid
    niceR Working = "Working"
    niceR Unsolvable = "Unsolvable"

baz = fmap nice $ simulate @System (bundle . controller @3 @3) $
    L.replicate 3 Nothing <> [Just grid1] <> L.repeat Nothing
  where
    nice (grid, solved, cmd) = unlines
        [ -- show (bitCoerce @_ @(Sudoku 3 3) <$> grid)
          case cmd of
              Nothing -> "NoStack"
              Just Pop -> "Pop"
              Just (Push grid) -> "Push"
        ]

bar = fmap nice $ simulate @System (bundle . propagator @3 @3) $
    L.replicate 5 Nothing <> [Just grid1] <> L.repeat Nothing
  where
    -- nice (grid, prop_res) = showGrid grid
    nice (grid, prop_res) = show prop_res
