{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PolyKinds #-}
module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH
import Clash.Class.Counter

import Data.Word

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Cell
import Sudoku.Controller
import Sudoku.Solve (Solvable)
import Sudoku.Serial
import Format

type HSep fmt = fmt :++ " "
type VSep fmt = fmt :++ "\r\n"

-- type GridFormat n m = n *: VSep (m *: VSep ("| " :++ m *: (HSep Forward :* n :++ "| ")))
type GridFormat n m = n *: VSep (m *: VSep (m *: HSep (n *: HSep Forward)))
type SolutionFormat n m = If '!' (Drop :++ VSep "Unsolvable.") ("Solution:\r\n" :++ GridFormat n m)
type Number = While '0' Drop :++ Until '#' Forward :++ Drop
type WithTiming fmt = Wait :++ "Cycles: " :++ Number :++ ".\r\n" :++ fmt
type OutputFormat n m = WithTiming (SolutionFormat n m)

type Formattable n m = (1 <= n, 1 <= m)

board
    :: (HiddenClockResetEnable dom)
    => forall n m -> (Solvable n m, Textual n m, Formattable n m)
    => Circuit (Df dom Word8) (Df dom Word8)
board n m =
    Df.mapMaybe parseCell |>
    Circuit (controller @n @m) |>
    Df.map (either id showCell) |>
    format (Loop (OutputFormat n m))

createDomain vXilinxSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity
    :: "CLK_100MHZ" ::: Clock Dom100
    -> "RESET"      ::: Reset Dom100
    -> "RX"         ::: Signal Dom100 Bit
    -> "TX"         ::: Signal Dom100 Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    serialize 9600 (board 3 3)

makeTopEntity 'topEntity
