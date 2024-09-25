{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ViewPatterns, NumericUnderscores, TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH

import Data.Maybe
import Control.Monad.State
import Data.Proxy
import Data.Word

import Protocols
import qualified Protocols.Df as Df
import Clash.Cores.UART(uart, ValidBaud)

import Sudoku.Cell
import Sudoku.Solve
import Sudoku.Stack
import Format

type StackSize n m = ((n * m) * (m * n))
type Cnt n m = Index ((n * m) * (m * n))

data St n m
    = ShiftIn (Cnt n m)
    | Busy (Index (StackSize n m))
    | WaitPop (Index (StackSize n m))
    | WaitPush (Index (StackSize n m))
    | ShiftOut Bool (Cnt n m)
    deriving (Generic, NFDataX, Show, Eq)

data Control n m
    = Consume (Maybe (Cell n m))
    | Solve PropagatorCmd
    | Stack (StackCmd ())
    | Produce Bool (Cell n m)

controller'
    :: forall n m dom k. (Solvable n m)
    => (HiddenClockResetEnable dom)
    => Signal dom (Df.Data (Cell n m))
    -> Signal dom Ack
    -> ( Signal dom Ack
       , Signal dom (Df.Data (Cell n m))
       )
controller' shift_in out_ack = (in_ack, Df.maybeToData <$> shift_out)
  where
    (shift_in', shift_out, in_ack, propagator_cmd, stack_cmd) = mealySB step (ShiftIn @n @m 0) (Df.dataToMaybe <$> shift_in, out_ack, head_cell, register Progress result, sp)

    lines = \case
        Consume shift_in -> (shift_in, Nothing, Ack True, Nothing, Nothing)
        Solve cmd -> (Nothing, Nothing, Ack False, Just cmd, Nothing)
        Stack cmd -> (Nothing, Nothing, Ack False, Nothing, Just cmd)
        Produce proceed output -> (conflicted <$ guard proceed, Just output, Ack False, Nothing, Nothing)

    step (shift_in, out_ack, head_cell, result, sp) = fmap lines $ get >>= \case
        ShiftIn i -> do
            when (isJust shift_in) $ put $ maybe (Busy sp) ShiftIn $ countSuccChecked i
            pure $ Consume shift_in
        WaitPush top_sp -> do
            put $ Busy top_sp
            pure $ Solve CommitGuess
        WaitPop top_sp -> do
            put $ Busy top_sp
            pure $ Solve Propagate
        Busy top_sp -> case result of
            Stuck -> do
                put $ WaitPush top_sp
                pure $ Stack $ Push ()
            Failure -> do
                let underflow = sp == top_sp
                put $ if underflow then ShiftOut False 0 else WaitPop top_sp
                pure $ Stack Pop
            Progress -> do
                pure $ Solve Propagate
            Solved -> do
                put $ ShiftOut True 0
                pure $ Solve Propagate
        ShiftOut solved i -> do
            s <- get
            let (proceed, s') = case out_ack of
                    Ack True -> (True, maybe (ShiftIn 0) (ShiftOut solved) $ countSuccChecked i)
                    Ack False -> (False, s)
            put s'
            pure $ Produce proceed $ if solved then head_cell else conflicted

    (head_cell, result, next_guesses) = propagator propagator_cmd shift_in' popped
    popped = stack_rd

    (stack_rd, sp) = stack (SNat @(StackSize n m)) stack_cmd (bundle next_guesses)

controller
    :: forall n m dom. (Solvable n m)
    => (HiddenClockResetEnable dom)
    => Circuit (Df dom (Cell n m)) (Df dom (Cell n m))
controller = Circuit $ uncurry controller'

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

type GridFormat n m =
    ((((Forward :++ " ") :* n :++ " ") :* m :++ "\r\n") :* m :++ "\r\n") :* n

board
    :: forall n m dom. (HiddenClockResetEnable dom, Textual n m, Solvable n m)
    => SNat n
    -> SNat m
    -> Circuit (Df dom Word8) (Df dom Word8)
board n m =
    Df.mapMaybe parseCell |>
    controller @n @m |>
    Df.map showCell |>
    formatGrid n m

formatGrid
    :: forall n m dom. (HiddenClockResetEnable dom, Textual n m, Solvable n m)
    => SNat n
    -> SNat m
    -> Circuit (Df dom Word8) (Df dom Word8)
formatGrid n m = format (Proxy @(GridFormat n m))

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    snd . toSignals (serialize (SNat @9600) (board (SNat @3) (SNat @3))) . (, pure ())

makeTopEntity 'topEntity
