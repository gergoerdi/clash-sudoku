{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH
import Clash.Class.Counter

import Sudoku.Board

import RetroClash.Utils hiding (oneHot)
import RetroClash.SerialRx
import RetroClash.SerialTx

import Data.Bits
import Data.Maybe
import Control.Monad (guard)
import Control.Monad.Writer
import Control.Monad.State

others :: (1 <= n) => Vec n a -> Vec n (Vec (n - 1) a)
others (Cons x Nil) = Nil :> Nil
others (Cons x xs@(Cons _ _)) = xs :> map (x :>) (others xs)

simplify
    :: forall n k. (KnownNat n, 1 <= n, KnownNat k)
    => forall k0. (k ~ k0 + 1)
    => Vec n (Square k)
    -> WriterT (Any, All) Maybe (Vec n (Square k))
simplify xs = traverse (uncurry simplifySquare) (zip xs (others xs))
  where
    simplifySquare :: forall n. Square k -> Vec n (Square k) -> WriterT (Any, All) Maybe (Square k)
    simplifySquare x xs = do
        guard $ x' /= 0
        tell (Any $ x' /= x, All $ isUnique x')
        pure x'
      where
        x' = foldl f x xs
        f x y | Unique{} <- getUnique y = x .&. complement y
              | otherwise = x

propagate1
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> WriterT (Any, All) Maybe (Sudoku n m)
propagate1 s = do
    (s :: Sudoku n m) <- rowwise  simplify s
    (s :: Sudoku n m) <- columnwise simplify s
    (s :: Sudoku n m) <- squarewise simplify s
    return s

commit1
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> (Sudoku n m, Maybe (Sudoku n m))
commit1 s = (Sudoku next, Sudoku <$> after)
  where
    next = map (map fst) r
    after = traverse (traverse snd) r

    r = flip evalState False . traverse (traverse f) . getSudoku $ s

    f :: Square (n * m) -> State Bool (Square (n * m), Maybe (Square (n * m)))
    f x
      | Unique{} <- getUnique x = pure (x, Just x)
      | otherwise = do
            changed <- get
            let mb_i = elemIndex True (reverse . bitCoerce $ x)
            case (changed, mb_i) of
                (False, Just i) -> do
                    put True
                    let x' = oneHot i
                        x'' = x .&. complement x'
                    pure (x', x'' <$ guard (x'' /= 0))
                _ -> pure (x, Just x)

propagate
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> Maybe (Sudoku n m, Bool)
propagate s = do
    (s', (Any changed, All solved)) <- runWriterT $ propagate1 s
    if changed then propagate s' else pure (s', solved)

solve1
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> Maybe (Sudoku n m)
solve1 = (Just . fst . commit1) <=< (fmap fst . propagate)

data Phase n m
    = Init
    | Propagate (Sudoku n m)
    | Try (Sudoku n m)
    | Solved (Sudoku n m)
    deriving (Generic, NFDataX)

data StackCmd a
    = Pop
    | Push a
    deriving (Show, Generic, NFDataX)

solver1
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Maybe (Sudoku n m)
    -> State (Phase n m) (Maybe (Sudoku n m), Maybe (StackCmd (Sudoku n m)))
solver1 (Just popBoard) = do
    put $ Propagate popBoard
    return (Nothing, Nothing)
solver1 Nothing = get >>= \case
    Init -> do
        return (Nothing, Just Pop)
    Solved board -> do
        return (Just board, Nothing)
    Propagate board -> case runWriterT $ propagate1 board of
        Nothing -> do
            return (Nothing, Just Pop)
        Just (board', (Any changed, All solved)) -> do
            put $ if
                | solved    -> Solved board'
                | changed   -> Propagate board'
                | otherwise -> Try board'
            return (Nothing, Nothing)
    Try board -> do
        let (next, after) = commit1 board
        put $ Propagate next
        return (Nothing, Push <$> after)

data Result n m
    = Working
    | Solution (Sudoku n m)
    | Unsolvable
    deriving (Generic, NFDataX)
deriving instance (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8) => Show (Result n m)

stack
    :: forall n a. (KnownNat n, NFDataX a)
    => forall dom. (HiddenClockResetEnable dom)
    => SNat n
    -> a
    -> Signal dom (Maybe (StackCmd a))
    -> ( Signal dom (Maybe a)
       , Signal dom Bool
       )
stack size x0 cmd = (enable (delay False en) rd, underflow)
  where
    sp = register (0 :: Index n) sp'
    (sp', en, wr, underflow) = unbundle $ interpret <$> sp <*> cmd

    interpret sp cmd = case cmd of
        Nothing -> (sp, False, Nothing, False)
        Just Pop -> (sp - 1, True, Nothing, sp == 0)
        Just (Push x) -> (sp + 1, False, Just x, False)

    rd = blockRam (replicate size x0) sp' (packWrite <$> sp' <*> wr)

type StackSize n m = ((n * m) * (m * n))

circuit
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> Signal dom (Result n m)
circuit newBoard = result <$> underflow <*> solution
  where
    (solution, stackCmd) = mealyStateB solver1 Init (newBoard .<|>. stackRd)
    (stackRd, underflow) = stack (SNat @(StackSize n m)) (Sudoku . repeat . repeat $ 0) stackCmd

    result True _ = Unsolvable
    result False (Just board) = Solution board
    result False Nothing = Working

serialReader
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Maybe (Unsigned 8)
    -> State (Index ((n * m) * (m * n)), Vec ((n * m) * (m * n)) (Square (n * m))) (Maybe (Sudoku n m))
serialReader nextChar = do
    (ptr, buf) <- get
    case nextChar of
        Just char | char == ascii '_' || (ascii '0' <= char && char <= ascii '9') -> do
            let buf' = buf <<+ parseSquare char
            case succIdx ptr of
              Nothing -> do
                  put (0, buf')
                  return $ Just $ parseBoard $ buf'
              Just ptr' -> do
                  put (ptr', buf')
                  return $ Nothing
        otherwise -> do
            return Nothing

serialIn
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> Signal dom (Maybe (Sudoku n m))
serialIn = mealyState serialReader (0, repeat 0)

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
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k k'. (KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => forall k'. (KnownNat k', (n * m) * (m * n) ~ (k' + 1))
    => Bool
    -> Result n m
    -> State (Maybe (Ptr n m), Vec ((n * m) * (m * n)) (Square (n * m))) (Maybe (Unsigned 8))
serialWriter txReady result
    | not txReady = return Nothing
    | otherwise = do
          (ptr, buf) <- get
          case ptr of
              Nothing -> do
                  case result of
                      Working -> put (Nothing, repeat 0)
                      Unsolvable -> put (Just startPtr, repeat 0)
                      Solution board -> put (Just startPtr, concat . getSudoku $ board)
                  return Nothing
              Just ptr -> do
                  -- () <- traceShowM ptr
                  let (x, buf')
                          | (_, Right{}) <- ptr = (ascii '\n', buf)
                          | (_, Left (_, Right{})) <- ptr = (ascii '\n', buf)
                          | (_, Left (_, Left (_, Right{}))) <- ptr = (ascii ' ', buf)
                          | (_, Left (_, Left (_, Left (_, Right{})))) <- ptr = (ascii ' ', buf)
                          | otherwise = (showSquare $ head buf, buf <<+ 0)
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
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => forall k'. (KnownNat k', (n * m) * (m * n) ~ (k' + 1))
    => Bool
    -> Result n m
    -> State (Maybe (Ptr n m), Vec ((n * m) * (m * n)) (Square (n * m))) (Maybe (Unsigned 8), Bool)
serialWriter' txReady result = do
    x <- serialWriter txReady result
    ready <- gets $ isNothing . fst
    return (x, ready)

serialOut
    :: forall n m k k'. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1), k <= 8, KnownNat k', (n * m) * (m * n) ~ (k' + 1))
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Result n m)
    -> ( Signal dom (Maybe (Unsigned 8))
      , Signal dom Bool
      )
serialOut = curry $ mealyStateB (uncurry serialWriter') (Nothing, repeat 0)

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity = withEnableGen board
  where
    board rx = tx
      where
        inByte = fmap unpack <$> serialRx @8 @9600 @System (SNat @9600) rx
        (tx, txReady) = serialTx (SNat @9600) (fmap pack <$> outByte)

        (outByte, outReady) = serialOut txReady . circuit @3 @3 . serialIn $ inByte

makeTopEntity 'topEntity
