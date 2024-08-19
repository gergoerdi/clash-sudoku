{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ViewPatterns, NumericUnderscores, TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
module Sudoku where

import Clash.Prelude hiding (lift, mapAccumR)
import Clash.Class.Counter
import Clash.Annotations.TH

import Data.Maybe
import Control.Monad.State
import Data.Proxy
import Data.Char (chr)
import Data.Word

import Protocols
import qualified Protocols.Df as Df
import Clash.Cores.UART(uart, ValidBaud)

import RetroClash.Utils

import Sudoku.Grid
import Sudoku.Solve
import Sudoku.Stack
import Sudoku.Hacks
import Format

-- import Debug.Trace

type StackSize n m = ((n * m) * (m * n))
type Cnt n m = Index ((n * m) * (m * n))

type Digit = Index 10
newtype BCD n = BCD{ bcdDigits :: Vec n Digit }
    deriving (Generic, NFDataX, Eq, Show)

bcdMin :: (KnownNat n, 1 <= n) => BCD n
bcdMin = BCD $ repeat 0

bcdSucc :: (KnownNat n, 1 <= n) => BCD n -> BCD n
bcdSucc = BCD . snd . mapAccumR f 1 . bcdDigits
  where
    f :: Unsigned 1 -> Digit -> (Unsigned 1, Digit)
    f c d = let (c', d', _) = countSucc (0, d, c) in (c', d')

showDigit :: Digit -> Word8
showDigit n = ascii '0' + fromIntegral n

data St n m
    = ShiftIn (Cnt n m)
    | Busy (Index (StackSize n m)) (BCD 6)
    | WaitPush (Index (StackSize n m)) (BCD 6)
    | ShiftOutCycles Bool (BCD 6) (Index 6)
    | ShiftOutCyclesDelim Bool
    | ShiftOut Bool (Cnt n m)
    deriving (Generic, NFDataX, Show, Eq)


showGrid :: forall n m. (Showable n m) => Sudoku n m -> String
showGrid =
    fmap (chr . fromIntegral) .
    formatModel (Proxy @(GridFormat n m)) .
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
       , Signal dom (Df.Data (Either Word8 (Cell n m)))
       )
controller' shift_in out_ack = (in_ack, Df.maybeToData <$> shift_out)
  where
    (shift_in', shift_out, in_ack, propagator_cmd, stack_cmd) = mealyStateB step (ShiftIn @n @m 0) (Df.dataToMaybe <$> shift_in, out_ack, head_cell, register Progress result, sp)

    step (shift_in, out_ack, head_cell, result, sp) = do
        get >>= \case
            ShiftIn i -> do
                when (isJust shift_in) $ put $ maybe (Busy sp bcdMin) ShiftIn $ countSuccChecked i
                pure (shift_in, Nothing, Ack True, Nothing, Nothing)
            WaitPush top_sp cycles -> do
                let cycles' = bcdSucc cycles
                put $ Busy top_sp cycles'
                pure (Nothing, Nothing, Ack False, Just CommitGuess, Just $ Push ())
            Busy top_sp cycles -> do
                let cycles' = bcdSucc cycles
                case result of
                    Guess -> do
                        put $ WaitPush top_sp cycles'
                        pure (Nothing, Nothing, Ack False, Just Propagate, Just $ Push ())
                    Failure -> do
                        put $ Busy top_sp cycles'
                        let underflow = sp == top_sp
                        when underflow do
                            put $ ShiftOutCycles False cycles 0
                        pure (Nothing, Nothing, Ack False, Just Propagate, Pop <$ guard (not underflow))
                    Progress -> do
                        put $ Busy top_sp cycles'
                        pure (Nothing, Nothing, Ack False, Just Propagate, Nothing)
                    Solved -> do
                        put $ ShiftOutCycles True cycles 0
                        pure (Nothing, Nothing, Ack False, Just Propagate, Nothing)
            ShiftOutCycles solved cycles i -> do
                let cycles' = BCD . (`rotateLeftS` (SNat @1)) . bcdDigits $ cycles
                    s' = maybe (ShiftOutCyclesDelim solved) (ShiftOutCycles solved cycles') $ countSuccChecked i
                    output = showDigit . head . bcdDigits $ cycles
                when (case out_ack of Ack b -> b) $
                    put s'
                pure (Nothing, Just (Left output), Ack False, Nothing, Nothing)
            ShiftOutCyclesDelim solved -> do
                when (case out_ack of Ack b -> b) $
                    put $ ShiftOut solved 0
                pure (Nothing, Just (Left $ ascii '@'), Ack False, Nothing, Nothing)
            ShiftOut solved i -> do
                shift_in <- case out_ack of
                    Ack True -> do
                        put $ maybe (ShiftIn 0) (ShiftOut solved) $ countSuccChecked i
                        pure $ Just conflicted
                    _ -> do
                        pure Nothing
                let shift_out = Just $ Right $ if solved then head_cell else conflicted
                pure (shift_in, shift_out, Ack False, Nothing, Nothing)

    (head_cell, result, next_guesses) = propagator propagator_cmd shift_in' popped
    popped = stack_rd

    (stack_rd, sp) = stack (SNat @(StackSize n m)) stack_cmd (bundle next_guesses)

controller
    :: forall n m dom. (Solvable n m)
    => (HiddenClockResetEnable dom)
    => Circuit (Df dom (Cell n m)) (Df dom (Either Word8 (Cell n m)))
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

type OutputFormat n m =
    Wait :++ "Solver cycle count: " :++ Until '@' Forward :++ "\r\n" :++ GridFormat n m

board
    :: forall n m dom. (HiddenClockResetEnable dom, Readable n m, Showable n m, Solvable n m)
    => SNat n
    -> SNat m
    -> Circuit (Df dom Word8) (Df dom Word8)
board n m =
    Df.mapMaybe parseCell |>
    controller @n @m |>
    Df.map (either id showCell) |> format (Proxy @(OutputFormat n m))

formatGrid
    :: forall n m dom. (HiddenClockResetEnable dom, Readable n m, Showable n m, Solvable n m)
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
