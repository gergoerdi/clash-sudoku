{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ApplicativeDo, NumericUnderscores #-}
module Sudoku4 where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH

import Data.Traversable (for)
import Control.Arrow (second, (***))
import Data.Maybe
import Control.Monad.State
import qualified Data.List as L
import qualified Clash.Sized.Vector as V
import Data.Char (ord, chr)

import Protocols
import Protocols.Internal (mapCircuit, simulateCSE, CSignal(..))
import qualified Protocols.Df as Df
import Clash.Cores.UART(uart, ValidBaud)

ascii :: Char -> Unsigned 8
ascii = fromIntegral . ord

byteCircuit
    :: (HiddenClockResetEnable dom)
    => Circuit (Df dom (Unsigned 8)) (Df dom (Unsigned 8))
byteCircuit = punctuate
  where
    punctuate = Df.expander False \case
        False -> \x -> (True, x, False)
        True -> \_ -> (False, ascii ',', True)

-- From git@github.com:bittide/bittide-hardware.git
uartDf ::
  (HiddenClockResetEnable dom, ValidBaud dom baud) =>
  SNat baud ->
  Circuit
    (Df dom (BitVector 8), CSignal dom Bit)
    (CSignal dom (Maybe (BitVector 8)), CSignal dom Bit)
uartDf baud = Circuit \((request, rx_bit), _out_ack) ->
    let (received, tx_bit, in_ack) = uart baud rx_bit (Df.dataToMaybe <$> request)
    in ((Ack <$> in_ack, pure ()), (received, tx_bit))

byteCircuit'
    :: (HiddenClockResetEnable dom)
    => ( Signal dom (Maybe (Unsigned 8))
       , Signal dom Bool
       )
    -> ( Signal dom Bool
       , Signal dom (Maybe (Unsigned 8))
       )
byteCircuit' (in_byte, out_ack) = (in_ack, out_byte)
  where
    (in_ack', out_byte') = toSignals byteCircuit (in_byte', out_ack')

    in_byte' = Df.maybeToData <$> in_byte
    out_ack' = Ack <$> out_ack
    in_ack = (\case { Ack b -> b }) <$> in_ack'
    out_byte = Df.dataToMaybe <$> out_byte'

sim_byteCircuit = simulateCSE (exposeClockResetEnable $ byteCircuit @System)

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
    dataBits = L.map (slow . lsb) $ L.take 8 $ L.iterate (`shiftR` 1) x
    stopBit = slow high

model_encodeSerials :: Int -> [Unsigned 8] -> [Bit]
model_encodeSerials stretch xs =
    L.concatMap (\x -> model_encodeSerial stretch x <> L.replicate (10 * 5 * stretch) high) xs
    <> L.repeat high

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

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "ENABLE"     ::: Enable System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity clk rst en = withClockResetEnable clk rst en sudoku
  where
    sudoku rx = tx
      where
        ((out_ack, _), (in_bv, tx)) = toSignals (uartDf (SNat @5_000_000)) ((fmap pack <$> out_byte, rx), (pure (), pure ()))

        (_, in_byte) = toSignals (buffer |> Df.map unpack) (in_bv, out_ack)
        -- in_byte = fmap unpack . Df.maybeToData <$> in_bv

        out_byte :: Signal System (Df.Data (Unsigned 8))
        (in_ack, out_byte) = toSignals byteCircuit (in_byte, out_ack)
        -- out_byte = in_byte

makeTopEntity 'topEntity

sim =
    L.map (chr . fromIntegral) .
    model_decodeSerial 20 .
    simulate @System (hideClockResetEnable topEntity) .
    model_encodeSerials 20 $
    L.take 81 $ L.cycle . fmap ascii $ "Hello"
