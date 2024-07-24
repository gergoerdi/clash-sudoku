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
import Clash.Cores.UART(uart, ValidBaud)

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

type Dbg n m = (Sudoku n m, St n m)

controller'
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= m * m * m * n, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => Signal dom (Df.Data (Cell n m))
    -> Signal dom Ack
    -> ( Signal dom Ack
       , Signal dom (Df.Data (Cell n m))
       , Signal dom (Dbg n m)
       )
controller' shift_in out_ack = (in_ack, Df.maybeToData <$> shift_out, _dbg)
  where
    _dbg = bundle (bundle grid, st)

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
    => Circuit (Df dom (Cell n m)) (Df dom (Cell n m), CSignal dom (Dbg n m))
controller = Circuit \(shift_in, (out_ack, _)) ->
    let (in_ack, shift_out, dbg) = controller' shift_in out_ack
    in (in_ack, (shift_out, dbg))

-- From git@github.com:bittide/bittide-hardware.git
uartDf
    :: (HiddenClockResetEnable dom, ValidBaud dom baud)
    => SNat baud
    -> Circuit
        (Df dom (BitVector 8), CSignal dom Bit)
        (CSignal dom (Maybe (BitVector 8)), CSignal dom Bit)
uartDf baud = Circuit \((request, rx_bit), _out_ack) ->
    let (received, tx_bit, in_ack) = uart baud rx_bit (Df.dataToMaybe <$> request)
    in ((Ack <$> in_ack, pure ()), (received, tx_bit))

buffer :: (HiddenClockResetEnable dom, NFDataX a) => Circuit (CSignal dom (Maybe a)) (Df dom a)
buffer = Circuit \(x, ack) ->
    let r = register Nothing do
            current <- r
            next <- x
            ~(Ack ack) <- ack
            pure $ if ack then next else current <|> next
    in (pure (), Df.maybeToData <$> r)

serialize
    :: (HiddenClockResetEnable dom, ValidBaud dom baud, BitPack a, BitSize a ~ 8, BitPack b, BitSize b ~ 8)
    => SNat baud
    -> Circuit (Df dom a) (Df dom b)
    -> Circuit (CSignal dom Bit) (CSignal dom Bit)
serialize baud par_circuit = circuit \rx -> do
    (in_byte, tx) <- uartDf baud -< (out_byte, rx)
    out_byte <- Df.map pack <| par_circuit <| Df.map unpack <| buffer -< in_byte
    idC -< tx

board :: (HiddenClockResetEnable dom) => Circuit (Df dom (Unsigned 8)) (Df dom (Unsigned 8))
board = circuit \in_byte -> do
    (out_cell, _dbg) <- controller @3 @3 <| Df.mapMaybe parseCell -< in_byte
    Df.map (either ascii id) <| punctuate (punctuateGrid (SNat @3) (SNat @3)) <| Df.map showCell -< out_cell

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    snd . toSignals (serialize (SNat @9600) board) . (, pure ())

makeTopEntity 'topEntity
