{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE BlockArguments #-}
module Sudoku where

import Clash.Prelude hiding (lift, print, drop, until)
import Clash.Annotations.TH

import Data.Word

import Protocols
import qualified Protocols.Df as Df

import Sudoku.Cell
import Sudoku.Controller
import Sudoku.Solve (Solvable)
import Sudoku.Serial
import Format

eol = str "\r\n"

outputFormat :: forall n m -> (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Format Word8 Word8
outputFormat n m = wait <> cycles <> eol <> solution <> eol
  where
    cycles = str "Cycles: " <> number <> str "."
    solution = cond (== ascii '!') (drop <> str "Unsolvable.") (str "Solution:\r\n" <> grid)
    number = while (== ascii '0') drop <> until (== ascii '#') print <> drop
    grid = gridFormat n m

gridFormat :: forall n m -> (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Format Word8 Word8
gridFormat n m = n *: vsep (m *: vsep (m *: hsep (n *: hsep print)))
  where
    vsep fmt = fmt <> eol
    hsep fmt = fmt <> lit (ascii ' ')

type Formattable n m = (1 <= n, 1 <= m)

board
    :: (HiddenClockResetEnable dom)
    => forall n m -> (Solvable n m, Textual n m, Formattable n m)
    => Circuit (Df dom Word8) (Df dom Word8)
board n m =
    Df.mapMaybe parseCell |>
    Circuit (controller @n @m) |>
    Df.map (either id showCell) |>
    format (loop (outputFormat n m))

createDomain vXilinxSystem{vName="Dom100", vPeriod = hzToPeriod 100_000_000}

topEntity
    :: "CLK_100MHZ" ::: Clock Dom100
    -> "RESET"      ::: Reset Dom100
    -> "RX"         ::: Signal Dom100 Bit
    -> "TX"         ::: Signal Dom100 Bit
topEntity clk rst = withClockResetEnable clk rst enableGen $
    serialize 9600 (board 3 3)

makeTopEntity 'topEntity
