{-# LANGUAGE LambdaCase, MultiWayIf, ApplicativeDo, BlockArguments #-}
module Sudoku.Controller where

import Clash.Prelude
import Clash.Class.Counter

import Control.Monad.State.Strict

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Utils
import Sudoku.Cell
import Sudoku.Solve
import Format

type Serial dom a b = (Signal dom (Df.Data a), Signal dom Ack) -> (Signal dom Ack, Signal dom (Df.Data b))

controller
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Serial dom (Cell n m) (Cell n m)
controller (shift_in, out_ack) = (in_ack, shift_out)
  where
    (shift_in', shift_out, in_ack, enable_solver, stack_cmd) =
        mealySB (fmap lines . control) (ShiftIn 0) (shift_in, out_ack, head_cell, result)

    lines = \case
        WaitForIO -> (Nothing, Df.NoData, Ack False, False, Nothing)
        Consume shift_in -> (Just shift_in, Df.NoData, Ack True, False, Nothing)
        Solve -> (Nothing, Df.NoData, Ack False, True, Nothing)
        Stack mem_cmd -> (Nothing, Df.NoData, Ack False, True, Just mem_cmd)
        Produce proceed output -> (shift_in, Df.Data output, Ack False, False, Nothing)
          where
            shift_in = if proceed then Just conflicted else Nothing

    (result, head_cell, pushed) = solver shift_in' popped enable_solver
    popped = stack stack_cmd pushed

data MemCmd sz
    = Read (Index sz)
    | Write (Index sz)

type StackDepth n m = ((n * m) * (m * n))
type StackPtr n m = Index (StackDepth n m)
type CellIndex n m = Index ((n * m) * (m * n))

data St n m
    = ShiftIn (CellIndex n m)
    | Busy (StackPtr n m)
    | WaitPop (StackPtr n m)
    | ShiftOutSolved (CellIndex n m)
    | ShiftOutUnsolvable
    deriving (Generic, NFDataX)

data Control n m
    = WaitForIO
    | Consume (Cell n m)
    | Solve
    | Stack (MemCmd (StackDepth n m))
    | Produce Bool (Cell n m)

next :: (Counter a) => (a -> s) -> a -> s -> s
next cons i after = maybe after cons $ countSuccChecked i

control
    :: (Solvable n m)
    => (Df.Data (Cell n m), Ack, Cell n m, Result n m)
    -> State (St n m) (Control n m)
control (shift_in, out_ack, head_cell, result) = get >>= \case
    ShiftIn i -> case shift_in of
        Df.NoData -> do
            pure WaitForIO
        Df.Data shift_in -> do
            put $ next ShiftIn i (Busy 0)
            pure $ Consume shift_in
    WaitPop sp -> do
        put $ Busy sp
        pure Solve
    Busy sp -> case result of
        Blocked
            | sp == 0 -> do
                  put ShiftOutUnsolvable
                  pure WaitForIO
            | otherwise -> do
                  let sp' = sp - 1
                  put $ WaitPop sp'
                  pure $ Stack $ Read sp'
        Complete -> do
            put $ ShiftOutSolved 0
            pure WaitForIO
        Progress{} -> do
            pure Solve
        Stuck{} -> do
            put $ Busy (sp + 1)
            pure $ Stack $ Write sp
    ShiftOutUnsolvable -> do
        wait out_ack $ put $ ShiftIn 0
        pure $ Produce False $ conflicted
    ShiftOutSolved i -> do
        proceed <- wait out_ack $ put $ next ShiftOutSolved i (ShiftIn 0)
        pure $ Produce proceed $ head_cell
  where
    wait ack act = do
        s0 <- get
        s <- act *> get
        let (proceed, s') = case ack of
                Ack True -> (True, s)
                Ack False -> (False, s0)
        put s'
        pure proceed

stack
    :: forall sz dom a. (HiddenClockResetEnable dom, KnownNat sz, 1 <= sz, NFDataX a)
    => Signal dom (Maybe (MemCmd sz))
    -> Signal dom a
    -> Signal dom (Maybe a)
stack cmd push = enable (delay False rd) $ blockRamU NoClearOnReset (SNat @sz) undefined addr wr
  where
    (addr, rd, wr) = unbundle $ do
        cmd <- cmd
        push <- push
        pure $ case cmd of
            Nothing -> (undefined, False, Nothing)
            Just (Read addr) -> (addr, True, Nothing)
            Just (Write addr) -> (undefined, False, Just (addr, push))
