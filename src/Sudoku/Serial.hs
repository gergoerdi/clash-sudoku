{-# LANGUAGE TupleSections, ViewPatterns #-}
module Sudoku.Serial where

import Clash.Prelude hiding (lift)
import Clash.Class.Counter

import Sudoku.Board

import Data.Maybe
import Control.Monad (guard)
import Control.Monad.State

type Readable n m k = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= (n * m) * (m * n), (n * m) <= 9, KnownNat k, (n * m) ~ k + 1)

serialReader
    :: (Readable n m k)
    => Maybe (Unsigned 8)
    -> State (Index ((n * m) * (m * n)), Vec ((n * m) * (m * n)) (Space n m)) (Maybe (Sudoku n m))
serialReader nextChar = do
    (ptr, buf) <- get
    case parseSpace =<< nextChar of
        Just x -> do
            let buf' = buf <<+ x
            case countSuccChecked ptr of
              Nothing -> do
                  put (0, buf')
                  return $ Just $ unflattenBoard buf'
              Just ptr' -> do
                  put (ptr', buf')
                  return Nothing
        Nothing -> do
            return Nothing

type Sec n space a = (Index n, Either a (Index space))
type Ptr n m = Sec n 2 (Sec m 2 (Sec m 1 (Sec n 1 (Index 1))))

startPtr :: (KnownNat n, KnownNat m) => Ptr n m
startPtr =
    (minBound,) . Left $
    (minBound,) . Left $
    (minBound,) . Left $
    (minBound,) . Left $
    minBound

type Writeable n m k k' = (Readable n m k, KnownNat k', (n * m * m * n) ~ k' + 1)

serialWriter
    :: (Writeable n m k k')
    => Bool
    -> Maybe (Sudoku n m)
    -> State (Maybe (Ptr n m), Vec (n * m * m * n) (Space n m)) (Maybe (Unsigned 8))
serialWriter txReady load
    | not txReady = return Nothing
    | otherwise = do
          (ptr, buf) <- get
          case ptr of
              Nothing -> do
                  case load of
                      Nothing -> put (Nothing, pure conflicted)
                      Just new_board -> put (Just startPtr, flattenBoard new_board)
                  return Nothing
              Just ptr -> do
                  -- () <- traceShowM ptr
                  let ptr' = countSuccChecked ptr
                  case punctuate ptr of
                      Just punctuation -> do
                          put (ptr', buf)
                          return $ Just punctuation
                      Nothing -> do
                          put (ptr', buf <<+ conflicted)
                          return $ Just $ showSpace $ head buf
  where
    punctuate ptr
        | (_, Right 0) <- ptr                               = Just $ ascii '\r'
        | (_, Right 1) <- ptr                               = Just $ ascii '\n'
        | (_, Left (_, Right 0)) <- ptr                     = Just $ ascii '\r'
        | (_, Left (_, Right 1)) <- ptr                     = Just $ ascii '\n'
        | (_, Left (_, Left (_, Right{}))) <- ptr           = Just $ ascii ' '
        | (_, Left (_, Left (_, Left (_, Right{})))) <- ptr = Just $ ascii ' '
        | (_, Left (_, Left (_, Left (_, Left{})))) <- ptr  = Nothing

countSuccChecked :: Counter a => a -> Maybe a
countSuccChecked x = x' <$ guard (not overflow)
  where
    (unpack -> overflow, x') = countSucc (pack False, x)

serialWriter'
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, (n * m) <= 9)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => forall k'. (KnownNat k', (n * m * m * n) ~ k' + 1)
    => Bool
    -> Maybe (Sudoku n m)
    -> State (Maybe (Ptr n m), Vec (n * m * m * n) (Space n m)) (Maybe (Unsigned 8), Bool)
serialWriter' txReady load = do
    x <- serialWriter txReady load
    ready <- gets $ isNothing . fst
    return (x, ready)
