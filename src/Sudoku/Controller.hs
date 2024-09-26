{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ViewPatterns, NumericUnderscores, TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Controller where

import Clash.Prelude
import RetroClash.Utils (enable, packWrite)

import Data.Maybe
import Control.Monad.State

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Grid
import Sudoku.Cell
import Sudoku.Solve
import Format

type StackDepth n m = ((n * m) * (m * n))
type Cnt n m = Index ((n * m) * (m * n))

data MemCmd n a
    = Read (Index n)
    | Write (Index n) a

data St n m
    = ShiftIn (Cnt n m)
    | Settle (Index (StackDepth n m))
    | Busy (Index (StackDepth n m))
    | WaitPop (Index (StackDepth n m))
    | WaitPush (Index (StackDepth n m))
    | ShiftOut Bool (Cnt n m)
    deriving (Generic, NFDataX, Show, Eq)

data Control n m
    = Consume (Maybe (Cell n m))
    | Solve PropagatorCmd
    | Stack (MemCmd (StackDepth n m) (Sudoku n m))
    | Produce Bool (Cell n m)

controller'
    :: forall n m dom k. (Solvable n m)
    => (HiddenClockResetEnable dom)
    => (Signal dom (Df.Data (Cell n m)), Signal dom Ack)
    -> (Signal dom Ack, Signal dom (Df.Data (Cell n m)))
controller' (shift_in, out_ack) = (in_ack, Df.maybeToData <$> shift_out)
  where
    (shift_in', shift_out, in_ack, propagator_cmd, stack_cmd) = mealySB step (ShiftIn @n @m 0) (Df.dataToMaybe <$> shift_in, out_ack, head_cell, next_guesses, register Progress result)

    lines = \case
        Consume shift_in -> (shift_in, Nothing, Ack True, Nothing, Nothing)
        Solve cmd -> (Nothing, Nothing, Ack False, Just cmd, Nothing)
        Stack cmd -> (Nothing, Nothing, Ack False, Nothing, Just cmd)
        Produce proceed output -> (conflicted <$ guard proceed, Just output, Ack False, Nothing, Nothing)

    step (shift_in, out_ack, head_cell, next_guesses, result) = fmap lines $ get >>= \case
        ShiftIn i -> do
            when (isJust shift_in) $ put $ maybe (Settle 0) ShiftIn $ countSuccChecked i
            pure $ Consume shift_in
        Settle sp -> do
            put $ Busy sp
            pure $ Solve Propagate
        WaitPush sp -> do
            put $ Busy sp
            pure $ Solve CommitGuess
        WaitPop sp -> do
            put $ Settle sp
            pure $ Solve Propagate
        Busy sp -> case result of
            Stuck -> do
                put $ WaitPush (sp + 1)
                pure $ Stack $ Write sp next_guesses
            Failure -> do
                let underflow = sp == 0
                    sp' = sp - 1
                put $ if underflow then ShiftOut False 0 else WaitPop sp'
                pure $ Stack $ Read sp'
            Progress -> do
                pure $ Solve Propagate
            Solved -> do
                put $ ShiftOut True 0
                pure $ Solve Propagate
        s@(ShiftOut solved i) -> do
            let (proceed, s') = case out_ack of
                    Ack True -> (True, maybe (ShiftIn 0) (ShiftOut solved) $ countSuccChecked i)
                    Ack False -> (False, s)
            put s'
            pure $ Produce proceed $ if solved then head_cell else conflicted

    (head_cell, result, next_guesses) = propagator propagator_cmd shift_in' popped

    popped = enable (delay False rd) $
        blockRamU NoClearOnReset (SNat @(StackDepth n m)) undefined sp (packWrite <$> sp <*> wr)
      where
        (sp, rd, wr) = unbundle $ do
            stack_cmd <- stack_cmd
            pure $ case stack_cmd of
                Nothing -> (0, False, Nothing)
                Just (Read sp) -> (sp, True, Nothing)
                Just (Write sp x) -> (sp, False, Just x)
