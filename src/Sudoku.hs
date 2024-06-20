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

circuit
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> Signal dom (Result n m)
circuit new_grid = result
  where
    (solved_grid, stack_cmd) = controller load
    load = new_grid .<|>. stack_rd
    (stack_rd, underflow) = stack (SNat @(StackSize n m)) emptySudoku stack_cmd'

    result = do
        busy <- busy
        new_grid <- isJust <$> new_grid
        underflow <- underflow
        solved_grid <- solved_grid
        pure $
          if
            | Just grid <- solved_grid  -> Solved grid
            | not busy && not new_grid -> Idle
            | underflow                -> Unsolvable
            | otherwise                -> Working

    stack_cmd' = guardA busy stack_cmd
    busy = register False $ isWorking <$> result

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity = withEnableGen grid
  where
    grid rx = tx
      where
        inByte = fmap unpack <$> serialRx @8 @9600 @System (SNat @9600) rx
        (tx, txReady) = serialTx (SNat @9600) (fmap pack <$> outByte)

        (outByte, outReady) = serialOut txReady . fmap fromResult . circuit @3 @3 . serialIn $ inByte

        fromResult Working = Nothing
        fromResult Unsolvable = Just emptySudoku
        fromResult (Solved grid) = Just grid

makeTopEntity 'topEntity
