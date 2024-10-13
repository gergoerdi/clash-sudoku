{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH

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
    :: forall dom. (HiddenClockResetEnable dom)
    => forall n m -> (Solvable n m, Textual n m)
    => Circuit (Df dom Word8) (Df dom Word8)
board n m =
    Df.mapMaybe parseCell |>
    controllerDf @n @m |>
    Df.map (either id showCell) |>
    formatGrid n m

formatGrid
    :: forall dom. forall n m
    -> (HiddenClockResetEnable dom, Textual n m, Solvable n m)
    => Circuit (Df dom Word8) (Df dom Word8)
formatGrid n m = format (OutputFormat n m)

topEntity
    :: "CLK_100MHZ" ::: Clock System
    -> "RESET"      ::: Reset System
    -> "RX"         ::: Signal System Bit
    -> "TX"         ::: Signal System Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    snd . toSignals (serialize (SNat @9600) (board 3 3)) . (, pure ())

makeTopEntity 'topEntity
