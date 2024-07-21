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

punctuate
    :: (HiddenClockResetEnable dom)
    => Circuit (Df dom (Unsigned 8)) (Df dom (Unsigned 8))
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

board :: (HiddenClockResetEnable System) => Bool -> Bool -> Signal System Bit -> Signal System Bit
board buffer_input apply_punctuation rx = tx
  where
    ((out_ack, _), (in_bv, tx)) = toSignals (uartDf (SNat @5_000_000)) ((fmap pack <$> out_byte, rx), (pure (), pure ()))

    in_byte = fmap unpack <$> in_bv

    in_byte'
        | buffer_input
        = let (_, in_byte') = toSignals buffer (in_byte, out_ack) in in_byte'

        | otherwise
        = Df.maybeToData <$> in_byte

    out_byte :: Signal System (Df.Data (Unsigned 8))
    out_byte
        | apply_punctuation
        = let (in_ack, out_byte) = toSignals punctuate (in_byte', out_ack) in out_byte

        | otherwise
        = in_byte'

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "ENABLE"     ::: Enable System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity clk rst en = withClockResetEnable clk rst en (board True True)

makeTopEntity 'topEntity

simParallel =
    fmap (chr . fromIntegral) .
    simulateCSE @System (exposeClockResetEnable punctuate) $
    L.take 81 $ L.cycle . fmap ascii $ "Hello"

simSerial =
    fmap (chr . fromIntegral) .
    model_decodeSerial 20 .
    simulate @System (hideClockResetEnable topEntity) .
    model_encodeSerials 20 $
    L.take 81 $ L.cycle . fmap ascii $ "Hello"
