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

import Sudoku.Matrix
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

simplify :: forall n k k0. (KnownNat n, 1 <= n, KnownNat k, k ~ k0 + 1) => Vec n (Square k) -> WriterT (Any, All) Maybe (Vec n (Square k))
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
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m -> WriterT (Any, All) Maybe (Sudoku n m)
propagate1 s = do
    (s :: Sudoku n m) <- fmap Sudoku . rowwise simplify . getSudoku $ s
    (s :: Sudoku n m) <- fmap Sudoku . columnwise simplify . getSudoku $ s
    (s :: Sudoku n m) <- fmap Sudoku . squarewise @n @m simplify . getSudoku $ s
    return s

commit1
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m -> (Sudoku n m, Maybe (Sudoku n m))
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
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m -> Maybe (Sudoku n m, Bool)
propagate s = do
    (s', (Any changed, All solved)) <- runWriterT $ propagate1 s
    if changed then propagate s' else pure (s', solved)

solve1
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m -> Maybe (Sudoku n m)
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
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1))
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
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
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
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => Maybe (Unsigned 8)
    -> State (Index ((n * m) * (m * n)), Vec ((n * m) * (m * n)) (Unsigned 8)) (Maybe (Sudoku n m))
serialReader = undefined

serialIn
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> Signal dom (Maybe (Sudoku n m))
serialIn nextChar = undefined

serialOut
    :: forall n m k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Result n m)
    -> Signal dom (Maybe (Unsigned 8))
serialOut outReady = undefined

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
        (tx, outReady) = serialTx (SNat @9600) (fmap pack <$> outByte)

        outByte = serialOut outReady . circuit @3 @3 . serialIn $ inByte

makeTopEntity 'topEntity
