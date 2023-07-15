module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH
import Clash.Class.Counter

import Sudoku.Board
import Sudoku.Serial
import Sudoku.Solve
import Sudoku.Stack

import RetroClash.Utils
import RetroClash.SerialRx
import RetroClash.SerialTx

type StackSize n m = ((n * m) * (m * n))

circuit
    :: forall n m dom k n' m'. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => (KnownNat k, (n * m) ~ (k + 1), 1 <= k)
    => (KnownNat m', m ~ m' + 1, KnownNat n', n ~ n' + 1)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Sudoku n m))
    -> Signal dom (Result n m)
circuit newBoard = result <$> underflow <*> solution
  where
    (solution, stackCmd) = mealyStateB solver1 Init (newBoard .<|>. stackRd)
    (stackRd, underflow) = stack (SNat @(StackSize n m)) (pure conflicted) stackCmd

    result True _ = Unsolvable
    result False (Just board) = Solution board
    result False Nothing = Working

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

        (outByte, outReady) = serialOut txReady . fmap fromResult . circuit @3 @3 . serialIn $ inByte

        fromResult Working = Nothing
        fromResult Unsolvable = Just emptySudoku
        fromResult (Solution board) = Just board

makeTopEntity 'topEntity
