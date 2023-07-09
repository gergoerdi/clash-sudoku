{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf #-}
module Main where

import Clash.Prelude hiding (lift)

import Sudoku
import Control.Monad.State

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

main :: IO ()
main = do
    return ()
