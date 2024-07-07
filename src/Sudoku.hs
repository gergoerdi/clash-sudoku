{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ApplicativeDo #-}
module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH
import Clash.Class.Counter

import Data.Traversable (for)
import Control.Arrow (second, (***))
import Data.Maybe

import Sudoku.Matrix
import Sudoku.Grid
import Sudoku.Serial
import Sudoku.Solve hiding (Propagate)
import Sudoku.Stack

import RetroClash.Utils
import RetroClash.SerialRx
import RetroClash.SerialTx

import Debug.Trace

type StackSize n m = ((n * m) * (m * n))

data Result n m
    = Idle
    | Working
    | Solved (Sudoku n m)
    | Unsolvable
    deriving (Generic, NFDataX)

isWorking :: Result n m -> Bool
isWorking = \case
    Working -> True
    _ -> False

data Result' = Idle' | Working' | Solved' | Unsolvable'
    deriving (Generic, NFDataX, Eq, Show)

circuit'
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Cell n m))
    -> Signal dom Bool
    -> ( Signal dom (Maybe Result')
      , Signal dom (Maybe (Cell n m))
      )
circuit' shift_in out_ready = undefined -- (result, shift_out)
  -- where


circuit
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom Result'
      , Grid n m (Signal dom (Cell n m))
      )
circuit new_grid = (result, grid)
  where
    (grid, is_solved, stack_cmd', next_guesses) = controller load
    load = new_grid .<|>. stack_rd
    (stack_rd, sp) = stack (SNat @(StackSize n m)) emptySudoku stack_cmd

    top_sp = regEn Nothing (isJust <$> new_grid) (Just <$> sp)

    result = do
        busy <- busy
        new_grid <- isJust <$> new_grid
        sp <- sp
        top_sp <- top_sp
        is_solved <- is_solved
        stack_cmd <- stack_cmd
        pure
          let underflow = case stack_cmd of
                  Just Pop -> top_sp == Just sp
                  _ -> False
          in if
            | is_solved                -> Solved'
            | not busy && not new_grid -> Idle'
            | underflow                -> Unsolvable'
            | otherwise                -> Working'

    stack_cmd = {- fmap (\x -> trace (show (fmap (() <$) x)) x) -} do
        busy <- busy
        cmd <- stack_cmd'
        next_guesses <- bundle next_guesses
        pure $ case cmd of
            _ | not busy -> Nothing
            Just Push' -> Just $ Push next_guesses
            Just Pop' -> Just Pop
            Nothing -> Nothing
    busy = register False $ result .== Working'

circuit''
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, n * m <= 9, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => SNat n
    -> SNat m
    -> Signal dom (Maybe (Unsigned 8))
    -> Signal dom Bool
    -> (Signal dom (Maybe (Unsigned 8)), Signal dom Result')
circuit'' _ _ in_byte tx_ready = (out_byte, result)
  where
    (result, solved_grid) = circuit @n @m . serialIn $ in_byte

    buffer = fmap (regMaybe conflicted . liftA2 f result) solved_grid

    f Solved' x = Just x
    f Unsolvable' x = Just conflicted
    f _ _ = Nothing

    (out_byte, out_ready) = serialOut tx_ready (enable (register False $ (result .== Solved' .||. result .== Unsolvable')) $ bundle buffer)

-- topEntity
--     :: "CLK_100MHZ" ::: Clock System
--     -> "RESET"      ::: Reset System
--     -> "RX"         ::: Signal System Bit
--     -> "TX"         ::: Signal System Bit
-- topEntity = withEnableGen grid
--   where
--     grid rx = tx
--       where
--         inByte = fmap unpack <$> serialRx @8 @9600 @System (SNat @9600) rx
--         (tx, txReady) = serialTx (SNat @9600) (fmap pack <$> outByte)

--         (result, solved_grid) = circuit @3 @3 . serialIn $ inByte
--         buffer = fmap (regMaybe conflicted . enable () solved_grid

--         (outByte, outReady) = serialOut txReady _ -- . fmap fromResult . circuit @3 @3 . serialIn $ inByte

--         -- fromResult Working = Nothing
--         -- fromResult Unsolvable = Just emptySudoku
--         -- fromResult (Solved grid) = Just grid

-- makeTopEntity 'topEntity
