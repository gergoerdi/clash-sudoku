{-# LANGUAGE RecordWildCards, OverloadedStrings, NumericUnderscores #-}
{-# LANGUAGE MultiWayIf, BlockArguments, LambdaCase #-}
module Main where

import Clash.Prelude

import Clash.Clashilator.FFI
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word
import qualified Data.List as L
import Data.Char
import Control.Arrow.Transformer.Automaton
import Data.Foldable

import Debug.Trace

import Sudoku.Sim
import Sudoku.Sim.Examples
import Sudoku (showGrid)
import Format (ascii)

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> do
    sim <- simInit
    let step input = do
            poke inp input
            simStep sim inp outp
            peek outp
    x <- act step
    simShutdown sim
    return x

board_rate = 100_000_000
serial_rate = 9_600
stretch = board_rate `div` serial_rate

main = withRunner $ \runCycle -> do
    let shiftIn b = do
            OUTPUT{..} <- runCycle INPUT
              { iRESET = low
              , iRX = b
              }
            pure oTX

    let rxs = encodeSerials stretch $ fmap ascii . showGrid $ hexodoku

    let go (Automaton step) = \case
            [] -> pure ()
            (x:xs) -> do
                y <- shiftIn x
                let (output, step') = step y
                traverse_ (putStr . (:[]) . chr . fromIntegral) output
                go step' xs

    go (decodeSerial stretch) rxs
