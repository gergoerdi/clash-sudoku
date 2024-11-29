{-# LANGUAGE NumericUnderscores #-}
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

type GridFormat n m = ((((Forward :++ " ") :* n :++ " ") :* m :++ "\r\n") :* m :++ "\r\n") :* n
type SolutionFormat n m = (If '!' "Unsolvable" (GridFormat n m)) :++ "\r\n"
type OutputFormat n m = SolutionFormat n m

board
    :: (HiddenClockResetEnable dom)
    => forall n m -> (Solvable n m, Textual n m)
    => Circuit (Df dom Word8) (Df dom Word8)
board n m =
    Df.mapMaybe parseCell |>
    Circuit (controller @n @m) |>
    Df.map showCell |>
    format (OutputFormat n m)

createDomain vXilinxSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity
    :: "CLK_100MHZ" ::: Clock Dom100
    -> "RESET"      ::: Reset Dom100
    -> "RX"         ::: Signal Dom100 Bit
    -> "TX"         ::: Signal Dom100 Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    serialize 9600 (board 3 3)

makeTopEntity 'topEntity
