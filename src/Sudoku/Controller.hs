{-# LANGUAGE LambdaCase, MultiWayIf, ApplicativeDo, BlockArguments #-}
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
import Format

type Digit = Index 10
type BCD n = Vec n Digit

type CyclesWidth (n :: Nat) (m :: Nat) = 6 -- TODO
type Cycles n m = BCD (CyclesWidth n m)

showDigit :: Digit -> Word8
showDigit n = ascii '0' + fromIntegral n

type StackDepth n m = ((n * m) * (m * n))
type StackPtr n m = Index (StackDepth n m)
type CellIndex n m = Index ((n * m) * (m * n))

data MemCmd n
    = Read (Index n)
    | Write (Index n)

data Phase n m
    = ShiftIn (CellIndex n m)
    | Busy
    | WaitPop
    | ShiftOutCycleCount Bool (Index (CyclesWidth n m))
    | ShiftOutCycleCountFinished Bool
    | ShiftOutSolved (CellIndex n m)
    | ShiftOutFailed
    deriving (Generic, NFDataX)

data St n m = St
    { phase :: Phase n m
    , cnt :: Cycles n m
    , sp :: StackPtr n m
    }
    deriving (Generic, NFDataX)

reset :: (KnownNat n, KnownNat m) => Phase n m -> St n m
reset phase = St{ phase = phase, cnt = countMin, sp = 0 }

goto :: Phase n m -> St n m -> St n m
goto phase st = st{ phase = phase }

tick :: State (St n m) a -> State (St n m) a
tick body = do
    modify \st@St{ cnt = cnt } -> st{ cnt = countSucc cnt }
    body

pop :: (Solvable n m) => State (St n m) (Maybe (StackPtr n m))
pop = do
    sp <- gets sp
    let sp' = countPredChecked sp
    modify \st@St{ sp = sp } -> st{ sp = fromMaybe sp sp' }
    pure sp'

push :: (Solvable n m) => State (St n m) (StackPtr n m)
push = do
    sp <- gets sp
    modify \st -> st{ sp = sp + 1 }
    pure sp

data Control n m
    = Consume (Maybe (Cell n m))
    | Solve Bool
    | Stack (MemCmd (StackDepth n m))
    | Produce Bool (Either Word8 (Cell n m))

type Serial dom a b = (Signal dom (Df.Data a), Signal dom Ack) -> (Signal dom Ack, Signal dom (Df.Data b))

controller
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Serial dom (Cell n m) (Either Word8 (Cell n m))
controller (shift_in, out_ack) = (in_ack, Df.maybeToData <$> shift_out)
  where
    (shift_in', shift_out, in_ack, solver_en, stack_cmd) =
        mealySB step
            (St{ phase = ShiftIn @n @m 0, cnt = countMin, sp = 0 })
            (Df.dataToMaybe <$> shift_in, out_ack, head_cell, result)

    lines = \case
        Consume shift_in -> (shift_in, Nothing, Ack True, False, Nothing)
        Solve en -> (Nothing, Nothing, Ack False, en, Nothing)
        Stack mem -> (Nothing, Nothing, Ack False, True, Just mem)
        Produce proceed output -> (shift_in, Just output, Ack False, False, Nothing)
          where
            shift_in = if proceed then Just conflicted else Nothing

    step (shift_in, out_ack, head_cell, result) = fmap lines $ gets phase >>= \case
        ShiftIn i -> do
            when (isJust shift_in) $ put $ reset $ maybe Busy ShiftIn $ countSuccChecked i
            pure $ Consume shift_in
        WaitPop -> tick do
            modify $ goto Busy
            pure $ Solve True
        Busy -> tick $ case result of
            Blocked -> pop >>= \case
                Nothing -> do
                    modify $ goto $ ShiftOutCycleCount False 0
                    pure $ Solve False
                Just sp'-> do
                    modify $ goto WaitPop
                    pure $ Stack $ Read sp'
            Complete -> do
                modify $ goto $ ShiftOutCycleCount True 0
                pure $ Solve False
            Progress{} -> do
                pure $ Solve True
            Stuck{} -> do
                sp <- push
                pure $ Stack $ Write sp
        ShiftOutCycleCount solved i -> do
            cnt <- gets cnt
            wait out_ack \st@St{ cnt = cnt } -> st
                { cnt = cnt `rotateLeftS` SNat @1
                , phase = maybe (ShiftOutCycleCountFinished solved) (ShiftOutCycleCount solved) $ countSuccChecked i
                }
            pure $ Produce False $ Left $ showDigit $ head cnt
        ShiftOutCycleCountFinished solved -> do
            wait out_ack $ goto $ if solved then ShiftOutSolved 0 else ShiftOutFailed
            pure $ Produce False $ Left $ ascii '@'
        ShiftOutFailed -> do
            wait out_ack $ goto $ ShiftIn 0
            pure $ Produce False $ Right conflicted
        ShiftOutSolved i -> do
            proceed <- wait out_ack $ goto $ maybe (ShiftIn 0) ShiftOutSolved $ countSuccChecked i
            pure $ Produce proceed $ Right head_cell

    wait ack f = do
        s <- get
        let (proceed, s') = case ack of
                Ack True -> (True, f s)
                Ack False -> (False, s)
        put s'
        pure proceed

    (result, head_cell, next_guesses) = solver shift_in' popped solver_en

    popped = enable (delay False rd) $
        blockRamU NoClearOnReset (SNat @(StackDepth n m)) undefined addr (packWrite <$> addr <*> wr)
      where
        (addr, rd, wr) = unbundle $ do
            stack_cmd <- stack_cmd
            next_guesses <- next_guesses
            pure $ case stack_cmd of
                Nothing -> (undefined, False, Nothing)
                Just (Read addr) -> (addr, True, Nothing)
                Just (Write addr) -> (addr, False, Just next_guesses)
