{-# LANGUAGE TupleSections, ViewPatterns, ApplicativeDo, BlockArguments #-}
module Sudoku.Serial where

import Clash.Prelude hiding (lift)
import Clash.Class.Counter
import RetroClash.Utils (enable, mealyStateB)

import Sudoku.Grid

import Data.Maybe
import Control.Monad (guard)
import Control.Monad.State

type Readable n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, (n * m) <= 9)

type SequentialPtr n m = Coord n m

serialIn
    :: forall n m dom. (Readable n m, HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> Signal dom (Maybe (Sudoku n m))
serialIn nextChar = enable ready buf'
  where
    ptr = register (minBound :: SequentialPtr n m) ptr''
    buf = register (pure conflicted) buf'

    nextCell = (parseCell =<<) <$> nextChar
    (buf', ptr') = unbundle $ do
        nextCell <- nextCell
        buf <- buf
        ptr@(i, j, k, l) <- ptr
        pure $ case nextCell of
            Nothing -> (buf, Just ptr)
            Just x -> (setGrid (i, j, k, l) x buf, countSuccChecked ptr)

    (ready, ptr'') = unbundle $ maybe (True, minBound) (False,) <$> ptr'

type Sec n space a = (Index n, Either a (Index space))
type Ptr n m = Sec n 2 (Sec m 2 (Sec m 1 (Sec n 1 (Index 1))))

startPtr :: (KnownNat n, KnownNat m) => Ptr n m
startPtr =
    (minBound,) . Left $
    (minBound,) . Left $
    (minBound,) . Left $
    (minBound,) . Left $
    minBound

type Writeable n m = (Readable n m, 1 <= n, 1 <= m)

serialWriter
    :: (Writeable n m)
    => Bool
    -> Maybe (Sudoku n m)
    -> State (Maybe (Ptr n m), Sudoku n m) (Maybe (Unsigned 8))
serialWriter txReady load
    | not txReady = return Nothing
    | otherwise = do
          (ptr, buf) <- get
          case ptr of
              Nothing -> do
                  case load of
                      Nothing -> put (Nothing, pure conflicted)
                      Just new_grid -> put (Just startPtr, new_grid)
                  return Nothing
              Just ptr -> do
                  -- () <- traceShowM ptr
                  let ptr' = countSuccChecked ptr
                  case punctuate ptr of
                      Left punctuation -> do
                          put (ptr', buf)
                          return $ Just punctuation
                      Right (i, j, k, l) -> do
                          put (ptr', buf)
                          return $ Just $ showCell $ gridAt buf (i, j, k, l)
  where
    punctuate ptr
        | (_, Right 0) <- ptr                               = Left $ ascii '\r'
        | (_, Right 1) <- ptr                               = Left $ ascii '\n'
        | (_, Left (_, Right 0)) <- ptr                     = Left $ ascii '\r'
        | (_, Left (_, Right 1)) <- ptr                     = Left $ ascii '\n'
        | (_, Left (_, Left (_, Right{}))) <- ptr           = Left $ ascii ' '
        | (_, Left (_, Left (_, Left (_, Right{})))) <- ptr = Left $ ascii ' '
        | (i, Left (j, Left (k, Left (l, Left{})))) <- ptr  = Right (i, j, k, l)

countSuccChecked :: Counter a => a -> Maybe a
countSuccChecked x = x' <$ guard (not overflow)
  where
    (unpack -> overflow, x') = countSucc (pack False, x)

serialOut
    :: forall n m dom. (Writeable n m, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Maybe (Unsigned 8))
      , Signal dom Bool
      )
serialOut = curry $ mealyStateB (uncurry serialWriter') (Nothing, pure conflicted)
  where
    serialWriter' :: Bool -> Maybe (Sudoku n m) -> State (Maybe (Ptr n m), Sudoku n m) (Maybe (Unsigned 8), Bool)
    serialWriter' txReady load = do
        x <- serialWriter txReady load
        ready <- gets $ isNothing . fst
        return (x, ready)
