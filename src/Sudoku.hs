{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-} {-# OPTIONS -Wno-partial-type-signatures #-}
module Sudoku where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH

import Data.Word
import Data.Char (ord, chr)

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Cell
import Sudoku.Controller
import Sudoku.Solve (Solvable)
import Sudoku.Serial
import Format

eol = str "\r\n"

outputFormat :: forall n m. _
outputFormat = Wait :++ cycles :++ eol :++ solution :++ eol
  where
    cycles = str "Cycles: " :++ number :++ str "."
    solution = If (== '!') (Drop :++ str "Unsolvable.") (str "Solution:\r\n" :++ grid)
    number = while (== '0') Drop :++ Until (== '#') Print :++ Drop
    grid = gridFormat @n @m

gridFormat :: forall n m. _
gridFormat = n *: vsep (m *: vsep (m *: hsep (n *: hsep Print)))
  where
    vsep fmt = fmt :++ eol
    hsep fmt = fmt :++ Lit ' '

type Formattable n m = (1 <= n, 1 <= m)

board
    :: (HiddenClockResetEnable dom)
    => forall n m -> (Solvable n m, Textual n m, Formattable n m)
    => Circuit (Df dom Word8) (Df dom Word8)
board n m =
    Df.mapMaybe parseCell |>
    Circuit (controller @n @m) |>
    Df.map (either id showCell) |>
    format (Loop (outputFormat @n @m)) |>
    Df.map (fromIntegral . ord)

createDomain vXilinxSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity
    :: "CLK_100MHZ" ::: Clock Dom100
    -> "RESET"      ::: Reset Dom100
    -> "RX"         ::: Signal Dom100 Bit
    -> "TX"         ::: Signal Dom100 Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    serialize 9600 (board 3 3)

makeTopEntity 'topEntity
