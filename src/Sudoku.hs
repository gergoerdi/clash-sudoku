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

serialIn
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1), k <= 8)
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> Signal dom (Maybe (Sudoku n m))
serialIn = mealyState serialReader (0, repeat 0)

serialOut
    :: forall n m k k'. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, KnownNat k, (n * m) ~ (k + 1), k <= 8, KnownNat k', (n * m) * (m * n) ~ (k' + 1))
    => forall dom. (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (Sudoku n m))
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

        (outByte, outReady) = serialOut txReady . fmap fromResult . circuit @3 @3 . serialIn $ inByte

        fromResult Working = Nothing
        fromResult Unsolvable = Just $ Sudoku $ repeat . repeat $ 0
        fromResult (Solution board) = Just board

makeTopEntity 'topEntity
