{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ViewPatterns, NumericUnderscores, TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH
import Clash.Class.Counter.Internal

import Data.Maybe
import Control.Monad.State

import Protocols
import qualified Protocols.Df as Df

import RetroClash.Utils

import Sudoku.Grid
import Sudoku.Solve hiding (Propagate, controller)
import Sudoku.Stack
import Punctuate

-- import Debug.Trace

countSuccChecked :: Counter a => a -> Maybe a
countSuccChecked x = x' <$ guard (not overflow)
  where
    (overflow, x') = countSuccOverflow x

type StackSize n m = ((n * m) * (m * n))
type Cnt n m = Index ((n * m) * (m * n))

data St n m
    = ShiftIn (Cnt n m)
    | Busy (Index (StackSize n m))
    | WaitPush (Index (StackSize n m))
    | ShiftOut (Cnt n m)
    deriving (Generic, NFDataX, Show, Eq)

controller'
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= m * m * m * n, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => Signal dom (Df.Data (Cell n m))
    -> Signal dom Ack
    -> ( Signal dom Ack
       , Signal dom (Df.Data (Cell n m))
       )
controller' shift_in out_ack = (in_ack, Df.maybeToData <$> shift_out)
  where
    (unbundle -> (shift_in', out_enabled, in_ack, enable_propagate, commit_guess, stack_cmd), st) = mealyStateB step (ShiftIn @n @m 0) (Df.dataToMaybe <$> shift_in, out_ack, result, sp, bundle next_guesses)
    shift_out = enable out_enabled head_cell

    step (shift_in, out_ack, result, sp, next_guesses) = do
      st <- get
      x <-
        get >>= {- (\x -> traceShowM (x, result) >> pure x) >>= -} \case
            ShiftIn i -> do
                when (isJust shift_in) $ put $ maybe (Busy sp) ShiftIn $ countSuccChecked i
                pure (shift_in, False, Ack True, False, False, Nothing)
            WaitPush top_sp -> do
                put $ Busy top_sp
                pure (Nothing, False, Ack False, True, True, Just $ Push next_guesses)
            Busy top_sp -> do
                case result of
                    Guess -> do
                        put $ WaitPush top_sp
                        pure (Nothing, False, Ack False, True, False, Just $ Push next_guesses)
                    Failure -> do
                        pure (Nothing, False, Ack False, True, False, Just Pop)
                    Progress -> do
                        pure (Nothing, False, Ack False, True, False, Nothing)
                    Solved -> do
                        put $ ShiftOut 0
                        pure (Nothing, False, Ack False, True, False, Nothing)
            ShiftOut i {- | Ack True <- out_ack -} -> do
                shift_in <- case out_ack of
                    Ack True -> do
                        put $ maybe (ShiftIn 0) ShiftOut $ countSuccChecked i
                        pure $ Just conflicted
                    _ -> do
                        pure Nothing
                pure (shift_in, True, Ack False, False, False, Nothing)
      pure (x, (st))

    (head_cell, result, grid, can_guess, next_guesses) = propagator (register False enable_propagate) (commit_guess) shift_in' popped
    popped = stack_rd

    (stack_rd, sp) = stack (SNat @(StackSize n m)) (emptySudoku @n @m) stack_cmd

controller
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= m * m * m * n, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => Circuit (Df dom (Cell n m)) (Df dom (Cell n m))
controller = Circuit \(shift_in, out_ack) -> controller' shift_in out_ack

board :: (HiddenClockResetEnable dom) => Circuit (Df dom (Unsigned 8)) (Df dom (Unsigned 8))
board =
    Df.mapMaybe (parseCell @3 @3)
    |> controller @3 @3
    |> Df.map showCell
    |> punctuate (punctuateGrid (SNat @3) (SNat @3)) |> Df.map (either ascii id)

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "ENABLE"     ::: Enable System
    -> "IN"         ::: Signal System (Maybe (Unsigned 8))
    -> "OUT"        ::: Signal System (Maybe (Unsigned 8))
topEntity clk rst en = withClockResetEnable clk rst en \in_byte ->
    let (_, out_byte) = toSignals board (Df.maybeToData <$> in_byte, pure $ Ack True)
    in Df.dataToMaybe <$> out_byte

makeTopEntity 'topEntity
