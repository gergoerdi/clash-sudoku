{-# LANGUAGE TupleSections #-}
module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH

import Data.Proxy
import Data.Word

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Cell
import Sudoku.Controller
import Sudoku.Solve (Solvable)
import Sudoku.Serial
import Format

controllerDf
    :: forall n m dom. (Solvable n m)
    => (HiddenClockResetEnable dom)
    => Circuit (Df dom (Cell n m)) (Df dom (Either Word8 (Cell n m)))
controllerDf = Circuit controller

type GridFormat n m = ((((Forward :++ " ") :* n :++ " ") :* m :++ "\r\n") :* m :++ "\r\n") :* n
type SolutionFormat n m = (If '!' "Unsolvable" (GridFormat n m)) :++ "\r\n"
type WithTiming fmt = Wait :++ "Cycles: " :++ Until '@' Forward :++ "\r\n" :++ fmt
type OutputFormat n m = WithTiming (SolutionFormat n m)

board
    :: forall n m dom. (HiddenClockResetEnable dom, Textual n m, Solvable n m)
    => SNat n
    -> SNat m
    -> Circuit (Df dom Word8) (Df dom Word8)
board n m =
    Df.mapMaybe parseCell |>
    controllerDf @n @m |>
    Df.map (either id showCell) |>
    formatGrid n m

formatGrid
    :: forall n m dom. (HiddenClockResetEnable dom, Textual n m, Solvable n m)
    => SNat n
    -> SNat m
    -> Circuit (Df dom Word8) (Df dom Word8)
formatGrid n m = format (Proxy @(OutputFormat n m))

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    snd . toSignals (serialize (SNat @9600) (board (SNat @3) (SNat @3))) . (, pure ())

makeTopEntity 'topEntity
