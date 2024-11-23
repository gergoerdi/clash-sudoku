{-# LANGUAGE MultiWayIf, BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
module Sudoku.Sim.Serial where

import Clash.Prelude

import Data.Word
import qualified Data.List as L
import Control.Arrow.Transformer.Automaton

encodeSerial :: Natural -> Word8 -> [Bit]
encodeSerial stretch x = mconcat
    [ pause
    , start
    , mconcat dataBits
    , stop
    ]
  where
    slow = L.replicate (fromIntegral stretch)

    pause = L.replicate 10 high
    start = slow low
    dataBits = L.map (slow . lsb) $ L.take 8 . L.iterate (`shiftR` 1) $ x
    stop = slow high

encodeSerials :: Natural -> [Word8] -> [Bit]
encodeSerials stretch xs = (<> L.repeat high) $ L.concatMap (encodeSerial stretch) xs

majority :: (Eq a) => a -> [a] -> Bool
majority x xs = L.length matches > L.length nonMatches
  where
    (matches, nonMatches) = L.partition (== x) xs

decodeSerialA :: Natural -> Automaton (->) Bit (Maybe Word8)
decodeSerialA stretch = wait
  where
    wait = Automaton \b -> if
      | b == low -> (Nothing, slowly start)
      | otherwise -> (Nothing, wait)

    start _ = (Nothing, slowly $ dataBits 8 0)

    dataBits 0 x bs = (Just x, wait)
    dataBits n x bs = (Nothing, slowly $ dataBits (n - 1) x'')
      where
        x' = x `shiftR` 1
        x'' = if majority high bs then x' `setBit` 7 else x'

    slowly :: ([a] -> (Maybe b, Automaton (->) a (Maybe b))) -> Automaton (->) a (Maybe b)
    slowly step = go (stretch - 1) []
      where
        go 1 xs = Automaton \x -> step xs
        go n xs = Automaton \x -> (Nothing, go (n - 1) (x:xs))

decodeSerial :: Natural -> [Bit] -> [Word8]
decodeSerial stretch = go (decodeSerialA stretch)
  where
    go (Automaton step) (b:bs)
        | Just x <- output = x : go sim bs
        | otherwise = go sim bs
      where
        (output, sim) = step b

simSerial :: forall dom. (KnownDomain dom) => Natural -> (HiddenClockResetEnable dom => Signal dom Bit -> Signal dom Bit) -> [Word8] -> [Word8]
simSerial dataRate circuit = decodeSerial stretch . simulate @dom circuit . encodeSerials stretch
  where
    stretch = fromIntegral $ ceiling $ clockRate / fromIntegral dataRate
    clockRate = periodToHz $ snatToNatural $ clockPeriod @dom
