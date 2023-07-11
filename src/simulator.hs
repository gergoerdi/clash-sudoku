{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf #-}
module Main where

import Clash.Prelude hiding (lift)

import Sudoku.Matrix
import Sudoku.Board
import Sudoku.Serial
import Sudoku.Solve
import Sudoku.Stack

import Control.Monad.Writer
import Control.Monad.State
import Data.Char (isDigit)
import Data.Char (chr)
import Data.Maybe (catMaybes)
import Control.Monad.Loops

readBoard :: String -> Maybe (Sudoku 3 3)
readBoard = flip evalState (0, repeat 0) . go
  where
    go [] = return Nothing
    go (c:cs) = do
        r <- serialReader . Just . ascii $ c
        maybe (go cs) (return . Just) r

showBoard :: Sudoku 3 3 -> String
showBoard board = fmap (chr . fromIntegral) . catMaybes $ flip evalState (Nothing, repeat 0) $ do
    let step = serialWriter' True
    (c, _) <- step (Just board)
    cs <- fmap fst <$> unfoldWhileM (\ (_, ready) -> not ready) (step Nothing)
    pure (c:cs)

propagate
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> Maybe (Sudoku n m, Bool)
propagate s = do
    (s', (Any changed, All solved)) <- runWriterT $ propagate1 s
    if changed then propagate s' else pure (s', solved)

solveRec
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m -> Maybe (Sudoku n m)
solveRec s = do
    (s' , solved) <- propagate s
    if solved then pure s' else do
        let (next, after) = commit1 s'
        (solveRec next) <|> (solveRec =<< after)

solver
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Maybe (Sudoku n m)
    -> StateT (Bool, [Sudoku n m]) (State (Phase n m)) (Result n m)
solver newBoard = do
    read <- do
        (pop, ~(top:stack)) <- get
        if pop then do
            put (False, stack)
            return $ Just top
          else do
            return Nothing
    (result, cmd) <- lift $ solver1 (read <|> newBoard)
    case (result, cmd) of
      (Just board, _) -> do
          return $ Solution board
      (Nothing, Nothing) -> do
          return Working
      (Nothing, Just (Push board)) -> do
          modify \(_, stack) -> (False, board:stack)
          return Working
      (Nothing, Just Pop) -> do
          modify \(_, stack) -> (True, stack)
          return Working

solveStack
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> Maybe (Sudoku n m)
solveStack board = flip evalState Init $ flip evalStateT (False, mempty) $ do
    res <- solver (Just board)
    go res
  where
    go (Solution board) = return $ Just board
    go Unsolvable = return Nothing
    go Working = solver Nothing >>= go

board1 :: Sudoku 3 3
Just board1 = readBoard . unlines $
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

board2 :: Sudoku 3 3
Just board2 = readBoard . unlines $
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


main :: IO ()
main = do
    return ()
