{-# LANGUAGE LambdaCase, MultiWayIf, ApplicativeDo, BlockArguments #-}
module Sudoku.Controller where

import Clash.Prelude

import Data.Maybe
import Control.Monad
import Control.Monad.State.Strict

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Utils
import Sudoku.Cell
import Sudoku.Solve
import Format

type StackDepth n m = ((n * m) * (m * n))
type StackPtr n m = Index (StackDepth n m)
type CellIndex n m = Index ((n * m) * (m * n))

data MemCmd n
    = Read (Index n)
    | Write (Index n)

data St n m
    = ShiftIn (CellIndex n m)
    | Busy (StackPtr n m)
    | WaitPop (StackPtr n m)
    | ShiftOutSolved (CellIndex n m)
    | ShiftOutFailed
    deriving (Generic, NFDataX)

data Control n m
    = Consume (Maybe (Cell n m))
    | Solve Bool
    | Stack (MemCmd (StackDepth n m))
    | Produce Bool (Cell n m)

type Serial dom a b = (Signal dom (Df.Data a), Signal dom Ack) -> (Signal dom Ack, Signal dom (Df.Data b))

controller
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Serial dom (Cell n m) (Cell n m)
controller (shift_in, out_ack) = (in_ack, Df.maybeToData <$> shift_out)
  where
    (shift_in', shift_out, in_ack, solver_en, stack_cmd) =
        mealySB step (ShiftIn @n @m 0) (Df.dataToMaybe <$> shift_in, out_ack, head_cell, result)

    lines = \case
        Consume shift_in -> (shift_in, Nothing, Ack True, False, Nothing)
        Solve en -> (Nothing, Nothing, Ack False, en, Nothing)
        Stack mem -> (Nothing, Nothing, Ack False, True, Just mem)
        Produce proceed output -> (shift_in, Just output, Ack False, False, Nothing)
          where
            shift_in = if proceed then Just conflicted else Nothing

    step (shift_in, out_ack, head_cell, result) = fmap lines $ get >>= \case
        ShiftIn i -> do
            when (isJust shift_in) $ put $ maybe (Busy 0) ShiftIn $ countSuccChecked i
            pure $ Consume shift_in
        WaitPop sp -> do
            put $ Busy sp
            pure $ Solve True
        Busy sp -> case result of
            Blocked
                | sp == 0 -> do
                      put ShiftOutFailed
                      pure $ Solve False
                | otherwise -> do
                      let sp' = sp - 1
                      put $ WaitPop sp'
                      pure $ Stack $ Read sp'
            Complete -> do
                put $ ShiftOutSolved 0
                pure $ Solve False
            Progress{} -> do
                pure $ Solve True
            Stuck{} -> do
                put $ Busy (sp + 1)
                pure $ Stack $ Write sp
        ShiftOutFailed -> do
            wait out_ack $ ShiftIn 0
            pure $ Produce False $ conflicted
        ShiftOutSolved i -> do
            proceed <- wait out_ack $ maybe (ShiftIn 0) ShiftOutSolved $ countSuccChecked i
            pure $ Produce proceed $ head_cell

    wait ack s' = do
        s <- get
        let (proceed, s'') = case ack of
                Ack True -> (True, s')
                Ack False -> (False, s)
        put s''
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
