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

readGrid :: (Readable n m) => String -> Maybe (Sudoku n m)
readGrid s = consume input $ simulate @System serialIn input
  where
    input = fmap (Just . ascii) s

    consume :: [Maybe a] -> [Maybe b] -> Maybe b
    consume [] _ = Nothing
    consume (_:xs) (y:ys) = y <|> consume xs ys


showGrid :: (Writeable n m k) => Sudoku n m -> String
showGrid grid = toString . consume $ simulateB @System (serialOut (pure True)) input
  where
    input = Just grid : L.repeat Nothing

    consume ((c, ready):xs) = c : if ready then [] else consume xs
    toString = fmap (chr . fromIntegral) . catMaybes

printGrid :: (Writeable n m k) => Sudoku n m -> IO ()
printGrid = putStr . showGrid

propagate'
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> Maybe (Sudoku n m, Bool)
propagate' s = do
    (s', (Any changed, All solved)) <- runWriterT $ propagate s
    if changed then propagate' s' else pure (s', solved)

solveRec
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m -> Maybe (Sudoku n m)
solveRec s = do
    (s' , solved) <- propagate' s
    if solved then pure s' else do
        let (next, after) = commit1 s'
        (solveRec next) <|> (solveRec =<< after)

solver
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Maybe (Sudoku n m)
    -> StateT (Bool, [Sudoku n m]) (State (Phase n m)) (Result n m)
solver newGrid = do
    read <- do
        (pop, ~(top:stack)) <- get
        if pop then do
            put (False, stack)
            return $ Just top
          else do
            return Nothing
    (result, cmd) <- lift $ solver1 (read <|> newGrid)
    case (result, cmd) of
      (Just grid, _) -> do
          return $ Solution grid
      (Nothing, Nothing) -> do
          return Working
      (Nothing, Just (Push grid)) -> do
          modify \(_, stack) -> (False, grid:stack)
          return Working
      (Nothing, Just Pop) -> do
          modify \(_, stack) -> (True, stack)
          return Working

solveStack
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> Maybe (Sudoku n m)
solveStack grid = flip evalState Init $ flip evalStateT (False, mempty) $ do
    res <- solver (Just grid)
    go res
  where
    go (Solution grid) = return $ Just grid
    go Unsolvable = return Nothing
    go Working = solver Nothing >>= go

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

main :: IO ()
main = do
    return ()
