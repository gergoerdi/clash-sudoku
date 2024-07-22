{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ApplicativeDo, NumericUnderscores, TupleSections #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
module Sudoku6 where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH
import Clash.Class.Counter

import Data.Maybe
import qualified Data.List as L
import Data.Char (ord, chr)
import Data.Traversable (for)
import Control.Arrow (second, (***))
import Control.Monad.State
import Control.Arrow.Transformer.Automaton
import qualified Data.List as L
import qualified Clash.Sized.Vector as V

import Protocols
import Protocols.Internal (mapCircuit, simulateCSE, CSignal(..))
import qualified Protocols.Df as Df
import Clash.Cores.UART(uart, ValidBaud)

import RetroClash.Utils

import Sudoku.Matrix
import Sudoku.Grid
import Sudoku.Solve hiding (Propagate, controller)
import Sudoku.Stack
import Sudoku.Serial (countSuccChecked)
import Sudoku.Serial (Readable, Writeable)
import Punctuate

import Debug.Trace

type StackSize n m = ((n * m) * (m * n))
type Cnt n m = Index ((n * m) * (m * n))

data St n m
    = ShiftIn (Cnt n m)
    | Busy (Index (StackSize n m))
    | WaitPush (Index (StackSize n m))
    | ShiftOut (Cnt n m)
    deriving (Generic, NFDataX, Show, Eq)

controller
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= m * m * m * n, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => Circuit (Df dom (Cell n m)) (Df dom (Cell n m))
controller = Circuit go
  where
    go (shift_in, out_ack) = (in_ack, Df.maybeToData <$> shift_out')
      where
      --   shift_in' :: Signal dom (Maybe (Cell n m))
        (shift_in', out_enabled, in_ack, enable_propagate, commit_guess, stack_cmd) = mealyStateB step (ShiftIn @n @m 0) (Df.dataToMaybe <$> shift_in, out_ack, {- register Progress -} result, sp)
        shift_out' = guardA out_enabled shift_out

        -- step :: (Maybe (Cell n m), Bool, PropagatorResult, Index (StackSize n m)) -> State (St n m) (Maybe (Cell n m), Bool, Bool, Sudoku n m -> Maybe (StackCmd (Sudoku n m)))
        step (shift_in, out_ack, result, sp) = do
            get >>= (\x -> {- traceShowM x >> -} pure x) >>= \case
                ShiftIn i -> do
                    when (isJust shift_in) $ put $ maybe (Busy sp) ShiftIn $ countSuccChecked i
                    pure (shift_in, False, Ack True, False, False, const Nothing)
                WaitPush top_sp -> do
                    put $ Busy top_sp
                    pure (Nothing, False, Ack False, True, True, Just . Push)
                Busy top_sp -> do
                    case result of
                        Guess -> do
                            put $ WaitPush top_sp
                            pure (Nothing, False, Ack False, True, False, Just . Push)
                        Failure -> do
                            pure (Nothing, False, Ack False, True, False, const $ Just Pop)
                        Progress -> do
                            pure (Nothing, False, Ack False, True, False, const Nothing)
                        Solved -> do
                            put $ ShiftOut 0
                            pure (Nothing, False, Ack False, True, False, const Nothing)
                ShiftOut i | Ack True <- out_ack -> do
                    put $ maybe (ShiftIn 0) ShiftOut $ countSuccChecked i
                    pure (Just conflicted, True, Ack False, False, False, const Nothing)
                ShiftOut i | otherwise -> do
                    pure (Nothing, True, Ack False, False, False, const Nothing)

        (shift_out, result, _grid, can_guess, next_guesses) = propagator (register False enable_propagate) (commit_guess) shift_in' popped
        popped = stack_rd

        -- stack_cmd' :: Signal dom (Maybe (StackCmd (Sudoku n m)))
        stack_cmd' = stack_cmd <*> bundle next_guesses

        -- sp :: Signal dom (Index (StackSize n m))
        (stack_rd, sp) = stack (SNat @(StackSize n m)) (emptySudoku @n @m) stack_cmd'

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

model_encodeSerial :: Int -> Unsigned 8 -> [Bit]
model_encodeSerial stretch x = mconcat
    [ pause
    , startBit
    , mconcat dataBits
    , stopBit
    ]
  where
    slow = L.replicate stretch
    pause = slow high
    startBit = slow low
    dataBits = L.map (slow . lsb) $ L.take 8 . L.iterate (`shiftR` 1) $ x
    stopBit = slow high

model_encodeSerials :: Int -> [Unsigned 8] -> [Bit]
model_encodeSerials stretch xs = (<> L.repeat high) $
    L.concatMap (\x -> model_encodeSerial stretch x <> L.replicate (10 * 5 * stretch) high) xs

model_decodeSerial :: Int -> [Bit] -> [Unsigned 8]
model_decodeSerial stretch = wait
  where
    wait [] = []
    wait bs@(b:bs')
      | b == low = start bs
      | otherwise = wait bs'

    start bs
      | (bs, bs') <- L.splitAt stretch bs
      = dataBits 8 0 bs'

    dataBits 0 x bs = x : end bs
    dataBits n x bs
      | (bs, bs') <- L.splitAt stretch bs
      , let x' = x `shiftR` 1
            x'' = if L.all (== high) bs then x' `setBit` 7 else x'
      = dataBits (n - 1) x'' bs'

    end bs = wait bs

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
board =
    Df.mapMaybe parseCell |>
    controller @3 @3 |>
    Df.map showCell |>
    punctuate (punctuateGrid (SNat @3) (SNat @3)) |>
    Df.map (either ascii id)

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "ENABLE"     ::: Enable System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity clk rst en = withClockResetEnable clk rst en $
    snd . toSignals (serialize (SNat @5_000_000) board) . (, pure ())

makeTopEntity 'topEntity

-- simSerial =
--     fmap (chr . fromIntegral) .
--     model_decodeSerial 20 .
--     simulate @System (hideClockResetEnable topEntity) .
--     model_encodeSerials 20 $
--     L.take 81 $ L.cycle . fmap ascii $ "Hello"

sim_board = simulateCSE @System $ exposeClockResetEnable board

readGrid :: forall n m. (Readable n m) => String -> Maybe (Sudoku n m)
readGrid = go []
  where
    go xs cs
        | Just cells <- V.fromList xs
        = Just $ gridFromRows $ unconcat (SNat @(m * n)) . reverse $ cells

        | (c:cs) <- cs
        = go (maybe xs (:xs) $ parseCell . ascii $ c) cs

        | [] <- cs
        = Nothing

showGrid :: forall n m. (Writeable n m) => Sudoku n m -> String
showGrid = punctuateModel (punctuateGrid (SNat @n) (SNat @m)) . fmap (chr . fromIntegral . showCell) . toList . concat . gridToRows

grid1 :: Sudoku 3 3
Just grid1 = readGrid . unlines $
    [ "0 2 0  9 0 8  0 0 0"
    , "8 7 0  0 0 1  0 5 4"
    , "5 0 6  4 0 0  0 1 0"
    , ""
    , "0 0 2  0 0 0  0 9 5"
    , "0 0 0  0 0 0  0 0 0"
    , "9 4 0  0 0 0  8 0 0"
    , ""
    , "0 8 0  0 0 4  5 0 3"
    , "1 3 0  2 0 0  0 8 6"
    , "0 0 0  3 0 7  0 2 0"
    ]
