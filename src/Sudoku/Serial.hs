{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
module Sudoku.Serial where

import Clash.Prelude hiding (lift)

import Protocols
import qualified Protocols.Df as Df
import Clash.Cores.UART(uart, ValidBaud)

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
