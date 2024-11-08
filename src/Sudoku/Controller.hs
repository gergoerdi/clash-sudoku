{-# LANGUAGE LambdaCase, MultiWayIf, ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
module Sudoku.Controller where

import Clash.Prelude
import Clash.Class.Counter

import Data.Maybe
import Control.Monad
import Control.Monad.State.Strict
import Data.Word

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Utils
import Sudoku.Cell
import Sudoku.Solve
import Sudoku.Grid
import Format

type Digit = Index 10
type BCD n = Vec n Digit

type CyclesWidth (n :: Nat) (m :: Nat) = 6 -- TODO
type Cycles n m = BCD (CyclesWidth n m)

showDigit :: Digit -> Word8
showDigit n = ascii '0' + fromIntegral n

type StackDepth n m = ((n * m) * (m * n))
type CellIndex n m = Index ((n * m) * (m * n))

data MemCmd n
    = Read (Index n)
    | Write (Index n)

data St n m
    = ShiftIn (CellIndex n m)
    | Settle   (Cycles n m) (Index (StackDepth n m))
    | Busy     (Cycles n m) (Index (StackDepth n m))
    | WaitPop  (Cycles n m) (Index (StackDepth n m))
    | WaitPush (Cycles n m) (Index (StackDepth n m))
    | ShiftOutCycleCount Bool (Cycles n m) (Index (CyclesWidth n m))
    | ShiftOutCycleCountFinished Bool
    | ShiftOutSolved (CellIndex n m)
    | ShiftOutFailed
    deriving (Generic, NFDataX, Show, Eq)

data Control n m
    = Consume (Maybe (Cell n m))
    | Solve
    | Stack (MemCmd (StackDepth n m))
    | Produce Bool (Either Word8 (Cell n m))

controller
    :: forall n m dom k. (Solvable n m, Textual n m)
    => (HiddenClockResetEnable dom)
    => (Signal dom (Df.Data (Cell n m)), Signal dom Ack)
    -> (Signal dom Ack, Signal dom (Df.Data (Either Word8 (Cell n m))))
controller (shift_in, out_ack) = (in_ack, Df.maybeToData <$> shift_out)
  where
    (shift_in', shift_out, in_ack, solve, stack_cmd) =
        mealySB step (ShiftIn @n @m 0) (Df.dataToMaybe <$> shift_in, out_ack, head_cell, register undefined result)

    lines = \case
        Consume shift_in -> (shift_in, Nothing, Ack True, False, Nothing)
        Solve -> (Nothing, Nothing, Ack False, True, Nothing)
        Stack cmd -> (Nothing, Nothing, Ack False, False, Just cmd)
        Produce proceed output -> (shift_in, Just output, Ack False, False, Nothing)
          where
            shift_in = if proceed then Just conflicted else Nothing

    step (shift_in, out_ack, head_cell, result) = fmap lines $ get >>= \case
        ShiftIn i -> do
            when (isJust shift_in) $ put $ maybe (Settle countMin 0) ShiftIn $ countSuccChecked i
            pure $ Consume shift_in
        Settle cnt sp -> do
            put $ Busy (countSucc cnt) sp
            pure Solve
        WaitPush cnt sp -> do
            put $ Busy (countSucc cnt) sp
            pure Solve
        WaitPop cnt sp -> do
            put $ Settle (countSucc cnt) sp
            pure Solve
        Busy cnt sp -> case result of
            Pruned -> do
                put $ Busy (countSucc cnt) sp
                pure Solve
            Complete -> do
                put $ ShiftOutCycleCount True (countSucc cnt) 0
                pure Solve
            Guess -> do
                put $ WaitPush (countSucc cnt) (sp + 1)
                pure $ Stack $ Write sp
            Blocked -> case countPredChecked sp of
                Nothing -> do
                    put $ ShiftOutCycleCount False (countSucc cnt) 0
                    pure Solve
                Just sp'-> do
                    put $ WaitPop (countSucc cnt) sp'
                    pure $ Stack $ Read sp'
        ShiftOutCycleCount solved cnt i -> do
            let cnt' = cnt `rotateLeftS` SNat @1
            wait out_ack $ maybe (ShiftOutCycleCountFinished solved) (ShiftOutCycleCount solved cnt') $ countSuccChecked i
            pure $ Produce False $ Left $ showDigit $ head cnt
        ShiftOutCycleCountFinished solved -> do
            wait out_ack $ if solved then ShiftOutSolved 0 else ShiftOutFailed
            pure $ Produce False $ Left $ ascii '@'
        ShiftOutFailed -> do
            wait out_ack $ ShiftIn 0
            pure $ Produce False $ Right conflicted
        ShiftOutSolved i -> do
            proceed <- wait out_ack $ maybe (ShiftIn 0) ShiftOutSolved $ countSuccChecked i
            pure $ Produce proceed $ Right head_cell

    wait ack s' = do
        s <- get
        let (proceed, s'') = case ack of
                Ack True -> (True, s')
                Ack False -> (False, s)
        put s''
        pure proceed

    grid = register (pure wild) grid'
    (unbundle -> (unbundle -> (result, register undefined -> grid2), grid')) = runState <$> (machine <$> shift_in' <*> popped <*> solve) <*> grid
    head_cell = headGrid @n @m <$> grid

    popped = enable (delay False rd) $
        blockRamU NoClearOnReset (SNat @(StackDepth n m)) undefined sp (packWrite <$> sp <*> wr)
      where
        (sp, rd, wr) = unbundle $ do
            stack_cmd <- stack_cmd
            grid2 <- grid2
            pure $ case stack_cmd of
                Nothing -> (0, False, Nothing)
                Just (Read sp) -> (sp, True, Nothing)
                Just (Write sp) -> (sp, False, Just grid2)
