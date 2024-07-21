{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ApplicativeDo, NumericUnderscores #-}
module Sudoku4 where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH

import Data.Maybe
import qualified Data.List as L
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
    -> Signal dom Bit
    -> Signal dom Bit
serialize baud circuit rx = tx
  where
    ((out_ack, _), (in_bv, tx)) = toSignals (uartDf baud) ((fmap pack <$> out_dat, rx), (pure (), pure ()))

    (_, in_dat) = toSignals (buffer |> Df.map unpack) (in_bv, out_ack)
    (in_ack, out_dat) = toSignals circuit (in_dat, out_ack)

board :: (HiddenClockResetEnable System) => Bool -> Signal System Bit -> Signal System Bit
board apply_punctuation = serialize (SNat @5_000_000) if
    | apply_punctuation
    -> punctuate

    | otherwise
    -> Df.map id

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "ENABLE"     ::: Enable System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity clk rst en = withClockResetEnable clk rst en (board True)

makeTopEntity 'topEntity

simParallel =
    fmap (chr . fromIntegral) .
    simulateCSE @System (exposeClockResetEnable punctuate) $
    L.take 81 $ L.cycle . fmap ascii $ "Hello"

simSerial apply_punctuation =
    fmap (chr . fromIntegral) .
    model_decodeSerial 20 .
    simulate @System (board apply_punctuation) .
    model_encodeSerials 20 $
    L.take 81 $ L.cycle . fmap ascii $ "Hello"
