{-# LANGUAGE LambdaCase, MultiWayIf, BlockArguments #-}
module Sudoku.Controller where

import Clash.Prelude
import Clash.Class.Counter

import Control.Monad.State.Strict
import Data.Maybe
import Data.Word

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Utils
import Sudoku.Cell
import Sudoku.Solve
import Format

type Stream dom a b = (Signal dom (Df.Data a), Signal dom Ack) -> (Signal dom Ack, Signal dom (Df.Data b))

controller
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Stream dom (Cell n m) (Either Word8 (Cell n m))
controller (shift_in, out_ack) = (in_ack, shift_out)
  where
    (shift_in', shift_out, in_ack, enable_solver, stack_cmd) =
        mealySB (fmap lines . control)
            (reset $ ShiftIn 0)
            (shift_in, out_ack, head_cell, result)

    lines = \case
        WaitForIO -> (Nothing, Df.NoData, Ack False, False, Nothing)
        Consume cell_in -> (Just cell_in, Df.NoData, Ack True, False, Nothing)
        Solve -> (Nothing, Df.NoData, Ack False, True, Nothing)
        Stack mem_cmd -> (Nothing, Df.NoData, Ack False, True, Just mem_cmd)
        Produce proceed cell_out -> (cell_in, Df.Data cell_out, Ack False, False, Nothing)
          where
            cell_in = if proceed then Just conflicted else Nothing

    (result, head_cell, pushed) = solver shift_in' popped enable_solver
    popped = stack stack_cmd pushed

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

data Phase n m
    = ShiftIn (CellIndex n m)
    | Busy
    | WaitPop
    | ShiftOutCycleCount Bool (Index (CyclesWidth n m))
    | ShiftOutCycleCountFinished Bool
    | ShiftOutSolved (CellIndex n m)
    | ShiftOutUnsolvable
    deriving (Generic, NFDataX)

data St n m = St
    { phase :: Phase n m
    , cnt :: Cycles n m
    , sp :: StackPtr n m
    }
    deriving (Generic, NFDataX)

reset :: (KnownNat n, KnownNat m) => Phase n m -> St n m
reset phase = St{ phase = phase, cnt = countMin, sp = 0 }

goto :: Phase n m -> State (St n m) ()
goto phase = modify \st -> st{ phase = phase }

tick :: State (St n m) a -> State (St n m) a
tick body = do
    modify \st@St{ cnt = cnt } -> st{ cnt = countSucc cnt }
    body

pop :: (Solvable n m) => State (St n m) (Maybe (StackPtr n m))
pop = do
    sp <- gets sp
    if sp == 0 then pure Nothing else do
        let sp' = sp - 1
        modify \st -> st{ sp = sp' }
        pure $ Just sp'

push :: (Solvable n m) => State (St n m) (StackPtr n m)
push = do
    sp <- gets sp
    modify \st -> st{ sp = sp + 1 }
    pure sp

data Control n m
    = WaitForIO
    | Consume (Cell n m)
    | Solve
    | Stack (MemCmd (StackDepth n m))
    | Produce Bool (Either Word8 (Cell n m))

next :: (Counter a) => (a -> s) -> a -> s -> s
next cons i after = maybe after cons $ countSuccChecked i

control
    :: (Solvable n m)
    => (Df.Data (Cell n m), Ack, Cell n m, Result n m)
    -> State (St n m) (Control n m)
control (shift_in, out_ack, head_cell, result) = gets phase >>= \case
    ShiftIn i -> case shift_in of
        Df.NoData -> do
            pure WaitForIO
        Df.Data shift_in -> do
            put $ reset $ next ShiftIn i Busy
            pure $ Consume shift_in
    WaitPop -> tick do
        goto Busy
        pure Solve
    Busy -> tick $ case result of
        Blocked -> pop >>= \case
            Nothing -> do
                goto $ ShiftOutCycleCount False 0
                pure WaitForIO
            Just sp -> do
                goto WaitPop
                pure $ Stack $ Read sp
        Complete -> do
            goto $ ShiftOutCycleCount True 0
            pure WaitForIO
        Progress{} -> do
            pure Solve
        Stuck{} -> push >>= \sp -> do
            goto Busy
            pure $ Stack $ Write sp
    ShiftOutCycleCount solved i -> do
        cnt <- gets cnt
        wait out_ack $ do
            modify \st@St{ cnt = cnt } -> st{ cnt = cnt `rotateLeftS` SNat @1 }
            goto $ next (ShiftOutCycleCount solved) i (ShiftOutCycleCountFinished solved)
        pure $ Produce False $ Left $ showDigit $ head cnt
    ShiftOutCycleCountFinished solved -> do
        wait out_ack $ goto $ if solved then ShiftOutSolved 0 else ShiftOutUnsolvable
        pure $ Produce False $ Left $ ascii '#'
    ShiftOutUnsolvable -> do
        wait out_ack $ goto $ ShiftIn 0
        pure $ Produce False $ Right conflicted
    ShiftOutSolved i -> do
        proceed <- wait out_ack $ goto $ next ShiftOutSolved i (ShiftIn 0)
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

stack
    :: forall sz dom a. (HiddenClockResetEnable dom, KnownNat sz, 1 <= sz, NFDataX a)
    => Signal dom (Maybe (MemCmd sz))
    -> Signal dom a
    -> Signal dom (Maybe a)
stack cmd push = enable enable_rd rd
  where
    (rd_addr, wr) = unbundle $ toRam <$> cmd <*> push
    enable_rd = delay False $ isJust <$> rd_addr
    rd = blockRamU NoClearOnReset (SNat @sz) undefined (fromMaybe undefined <$> rd_addr) wr

    toRam cmd push = case cmd of
        Nothing -> (Nothing, Nothing)
        Just (Read addr) -> (Just addr, Nothing)
        Just (Write addr) -> (Nothing, Just (addr, push))
