{-# LANGUAGE TupleSections #-}
module Sudoku.Serial where

import Clash.Prelude hiding (lift)
import Clash.Class.Counter
import RetroClash.Utils (succIdx)

import Sudoku.Board

import Data.Maybe
import Control.Monad (guard)
import Control.Monad.State

serialReader
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Maybe (Unsigned 8)
    -> State (Index ((n * m) * (m * n)), Vec ((n * m) * (m * n)) (Space n m)) (Maybe (Sudoku n m))
serialReader nextChar = do
    (ptr, buf) <- get
    case nextChar of
        Just char | char == ascii '_' || (ascii '0' <= char && char <= ascii '9') -> do
            let buf' = buf <<+ parseSpace char
            case succIdx ptr of
              Nothing -> do
                  put (0, buf')
                  return $ Just $ unflattenBoard buf'
              Just ptr' -> do
                  put (ptr', buf')
                  return $ Nothing
        otherwise -> do
            return Nothing

type Sec n space a = (Index n, Either a (Index space))
type Ptr n m = Sec n 1 (Sec m 1 (Sec m 1 (Sec n 1 (Index 1))))

startPtr :: (KnownNat n, KnownNat m) => Ptr n m
startPtr =
    (minBound,) . Left $
    (minBound,) . Left $
    (minBound,) . Left $
    (minBound,) . Left $
    minBound

serialWriter
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, (n * m) <= 9)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => forall k'. (KnownNat k', (n * m * m * n) ~ k' + 1)
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
                  let (x, buf')
                          | (_, Right{}) <- ptr = (ascii '\n', buf)
                          | (_, Left (_, Right{})) <- ptr = (ascii '\n', buf)
                          | (_, Left (_, Left (_, Right{}))) <- ptr = (ascii ' ', buf)
                          | (_, Left (_, Left (_, Left (_, Right{})))) <- ptr = (ascii ' ', buf)
                          | (_, Left (_, Left (_, Left (_, Left{})))) <- ptr = (showSpace $ head buf, buf <<+ conflicted)
                      ptr' = countSuccChecked ptr
                  put (ptr', buf')
                  return (Just x)
  where
    next :: forall n m. (KnownNat n, KnownNat m, 0 <= m) => Either (Index n) (Index m) -> Maybe (Either (Index n) (Index m))
    next (Left x) = Just $ maybe (Right 0) Left $ succIdx x
    next (Right y) = Right <$> succIdx y

    countSuccChecked cnt = cnt' <$ guard (cnt' /= startPtr)
      where
        cnt' = countSucc cnt

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
