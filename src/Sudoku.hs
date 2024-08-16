{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ViewPatterns, NumericUnderscores, TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH
import Clash.Class.Counter.Internal

import Data.Maybe
import Control.Monad.State
import Data.Proxy
import Data.Char (chr)
import Data.Word

import Protocols
import qualified Protocols.Df as Df
import Clash.Cores.UART(uart, ValidBaud)

import Sudoku.Grid
import Sudoku.Solve
import Sudoku.Stack
import Format

-- import Debug.Trace

type StackSize n m = ((n * m) * (m * n))
type Cnt n m = Index ((n * m) * (m * n))

data St n m
    = ShiftIn (Cnt n m)
    | Busy (Index (StackSize n m))
    | WaitPush (Index (StackSize n m))
    | ShiftOut Bool (Cnt n m)
    deriving (Generic, NFDataX, Show, Eq)


showGrid :: forall n m. (Showable n m) => Sudoku n m -> String
showGrid =
    fmap (chr . fromIntegral @Word8) .
    formatModel (gridFormat (SNat @n) (SNat @m)) .
    fmap showCell .
    toList . flattenGrid

instance (Showable n m) => Show (Sudoku n m) where
    show = showGrid

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

    step (shift_in, out_ack, head_cell, result, sp) = do
        get >>= \case
            ShiftIn i -> do
                when (isJust shift_in) $ put $ maybe (Busy sp) ShiftIn $ countSuccChecked i
                pure (shift_in, Nothing, Ack True, Nothing, Nothing)
            WaitPush top_sp -> do
                put $ Busy top_sp
                pure (Nothing, Nothing, Ack False, Just CommitGuess, Just $ Push ())
            Busy top_sp -> do
                case result of
                    Guess -> do
                        put $ WaitPush top_sp
                        pure (Nothing, Nothing, Ack False, Just Propagate, Just $ Push ())
                    Failure -> do
                        let underflow = sp == top_sp
                        when underflow do
                            put $ ShiftOut False 0
                        pure (Nothing, Nothing, Ack False, Just Propagate, Pop <$ guard (not underflow))
                    Progress -> do
                        pure (Nothing, Nothing, Ack False, Just Propagate, Nothing)
                    Solved -> do
                        put $ ShiftOut True 0
                        pure (Nothing, Nothing, Ack False, Just Propagate, Nothing)
            ShiftOut solved i -> do
                shift_in <- case out_ack of
                    Ack True -> do
                        put $ maybe (ShiftIn 0) (ShiftOut solved) $ countSuccChecked i
                        pure $ Just conflicted
                    _ -> do
                        pure Nothing
                let shift_out = Just $ if solved then head_cell else conflicted
                pure (shift_in, shift_out, Ack False, Nothing, Nothing)

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

type FormatGrid n m =
    ((((Forward :++ " ") :* n :++ " ") :* m :++ "\r\n") :* m :++ "\r\n") :* n

gridFormat :: SNat n -> SNat m -> Spec (FormatGrid n m) Word8 Word8
gridFormat _ _ = Rep (Rep (Rep (Rep (Forward :++ Lit) :++ Lit) :++ Lit) :++ Lit)

board
    :: forall n m dom. (HiddenClockResetEnable dom, Readable n m, Showable n m, Solvable n m)
    => SNat n
    -> SNat m
    -> Circuit (Df dom Word8) (Df dom Word8)
board n m =
    Df.mapMaybe parseCell |>
    controller @n @m |>
    Df.map showCell |> format (gridFormat n m)

formatGrid
    :: forall n m dom. (HiddenClockResetEnable dom, Readable n m, Showable n m, Solvable n m)
    => SNat n
    -> SNat m
    -> Circuit (Df dom Word8) (Df dom Word8)
formatGrid n m = format (gridFormat n m)

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    snd . toSignals (serialize (SNat @9600) (board (SNat @3) (SNat @3))) . (, pure ())

makeTopEntity 'topEntity

topEntity2
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "RX"         ::: Signal System (Df.Data Word8)
    -> "TX"         ::: Signal System (Df.Data Word8)
topEntity2 clk rst = withClockResetEnable clk rst enableGen $
    snd . toSignals (formatGrid (SNat @3) (SNat @3)) . (, pure $ Ack True)

makeTopEntity 'topEntity2
