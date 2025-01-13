{-# LANGUAGE LambdaCase, MultiWayIf, BlockArguments #-}
module Sudoku.Controller where

import Clash.Prelude
import Clash.Class.Counter

import Control.Monad.State.Strict
import Data.Word

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Cell
import Sudoku.Solve
import Format

type Stream dom a b = (Signal dom (Df.Data a), Signal dom Ack) -> (Signal dom Ack, Signal dom (Df.Data b))

controller
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Stream dom (Cell n m) (Either Word8 (Cell n m))
controller (shift_in, out_ack) = (in_ack, shift_out)
  where
    (shift_in', shift_out, in_ack, enable_solver) =
        mealySB (fmap lines . control)
            (ShiftIn 0)
            (shift_in, out_ack, head_cell, result)

    lines = \case
        WaitForIO -> (Nothing, Df.NoData, Ack False, False)
        Consume cell_in -> (Just cell_in, Df.NoData, Ack True, False)
        Solve -> (Nothing, Df.NoData, Ack False, True)
        Produce proceed cell_out -> (cell_in, Df.Data cell_out, Ack False, False)
          where
            cell_in = if proceed then Just conflicted else Nothing

    (head_cell, result) = solver shift_in' enable_solver

type Digit = Index 10
type BCD n = Vec n Digit

type CyclesWidth (n :: Nat) (m :: Nat) = 6 -- TODO
type Cycles n m = BCD (CyclesWidth n m)

showDigit :: Digit -> Word8
showDigit n = ascii '0' + fromIntegral n

data MemCmd sz
    = Read (Index sz)
    | Write (Index sz)

type StackDepth n m = ((n * m) * (m * n))
type StackPtr n m = Index (StackDepth n m)
type CellIndex n m = Index ((n * m) * (m * n))

data St n m
    = ShiftIn (CellIndex n m)
    | Busy (Cycles n m)
    | ShiftOutCycleCount Result (Cycles n m) (Index (CyclesWidth n m))
    | ShiftOutCycleCountFinished Result
    | ShiftOutSolved (CellIndex n m)
    | ShiftOutUnsolvable
    deriving (Generic, NFDataX, Show)

data Control n m
    = WaitForIO
    | Consume (Cell n m)
    | Solve
    | Produce Bool (Either Word8 (Cell n m))

next :: (Counter a) => (a -> s) -> a -> s -> s
next cons i after = maybe after cons $ countSuccChecked i

control
    :: (Solvable n m)
    => (Df.Data (Cell n m), Ack, Cell n m, Maybe Result)
    -> State (St n m) (Control n m)
control (shift_in, out_ack, head_cell, result) = get >>= {-(\x -> traceShowM x >> pure x) >>= -} \case
    ShiftIn i -> case shift_in of
        Df.NoData -> do
            pure WaitForIO
        Df.Data shift_in -> do
            put $ next ShiftIn i (Busy countMin)
            pure $ Consume shift_in
    Busy cnt -> do
        let cnt' = countSucc cnt
        put $ case result of
            Nothing -> Busy cnt'
            Just result -> ShiftOutCycleCount result cnt' 0
        pure Solve
    ShiftOutCycleCount result cnt i -> do
        let cnt' = cnt `rotateLeftS` d1
        wait out_ack $ do
            put $ next (ShiftOutCycleCount result cnt') i (ShiftOutCycleCountFinished result)
        pure $ Produce False $ Left $ showDigit $ head cnt
    ShiftOutCycleCountFinished result -> do
        wait out_ack $ put $ case result of
            Solved -> ShiftOutSolved 0
            Unsolvable -> ShiftOutUnsolvable
        pure $ Produce False $ Left $ ascii '#'
    ShiftOutUnsolvable -> do
        wait out_ack $ put $ ShiftIn 0
        pure $ Produce False $ Right conflicted
    ShiftOutSolved i -> do
        proceed <- wait out_ack $ put $ next ShiftOutSolved i (ShiftIn 0)
        pure $ Produce proceed $ Right head_cell
  where
    wait ack act = do
        s0 <- get
        s <- act *> get
        let (proceed, s') = case ack of
                Ack True -> (True, s)
                Ack False -> (False, s0)
        put s'
        pure proceed
