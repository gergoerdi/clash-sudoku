{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns, BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Clash.Prelude hiding (lift)
import Protocols.Internal (simulateCSE)
import qualified Protocols.Df as Df
import Protocols (Ack(..))

import Data.Char (chr)
import qualified Data.List as L
import qualified Clash.Sized.Vector as V
import Control.Arrow.Transformer.Automaton
import Control.Monad (guard)

import Sudoku.Iso
import Sudoku.Grid
import Sudoku.Cell
import Format (ascii)
import Format.Model
import Sudoku
import Sudoku.Controller
import Sudoku.Solve (Sudoku, Solvable)
import Sudoku.Sim.Serial
import Sudoku.Sim.Examples
import Text.Printf

showGrid :: forall n m. (Textual n m) => Sudoku n m -> String
showGrid =
    fmap (chr . fromIntegral) .
    formatModel (GridFormat n m) .
    fmap showCell .
    toList . embed flatGrid

instance (Textual n m) => Show (Sudoku n m) where
    show = showGrid

sim_board :: forall n m. (Solvable n m, Textual n m) => Sudoku n m -> String
sim_board =
    fmap (chr . fromIntegral) .
    simulateCSE @System (exposeClockResetEnable (board n m)) .
    fmap ascii . showGrid

sim_topEntity :: Sudoku 3 3 -> String
sim_topEntity =
    fmap (chr . fromIntegral) .
    simSerial dataRate (hideClockResetEnable \clk rst _en -> topEntity clk rst) .
    fmap ascii . showGrid @3 @3
  where
    dataRate = 9_600

solve :: forall n m. (Solvable n m, Textual n m) => Sudoku n m -> (Int, Maybe (Sudoku n m))
solve = start (signalAutomaton @System $ bundle . controller @n @m . unbundle) . toList . embed flatGrid
  where
    start (Automaton step) xs = load sim xs
      where
        (_, sim) = step (Df.NoData, Ack True)

    load (Automaton step) xs@(x:xs') = load sim $ if ack then xs' else xs
      where
        ((Ack ack, _), sim) = step (Df.Data x, Ack True)
    load sim [] = wait 0 sim

    wait !n (Automaton step) = case output of
        Df.Data{} -> (n, consume [] sim)
        Df.NoData -> wait (n + 1) sim
      where
        ((_, output), sim) = step (Df.NoData, Ack False)

    consume acc (Automaton step)
        | Just cells <- V.fromList (L.reverse acc)
        = Just $ project flatGrid cells

        | Df.Data (Right x) <- output
        = guard (x /= conflicted) *> consume (x : acc) sim

        | otherwise
        = consume acc sim
      where
        ((_, output), sim) = step (Df.NoData, Ack True)

checkSolved :: forall n m. (Solvable n m) => Sudoku n m -> Bool
checkSolved grid = validBy rows && validBy cols && validBy boxs
  where
    validBy f = all valid $ embed f grid
    valid xs = L.sort (toList xs) == L.sort [ given i | i <- [minBound..maxBound] ]

main :: IO ()
main = do
    test "grid1" grid1
    test "grid2" grid2
    test "hard" hard
    test "should be unsolvable" unsolvable
    test "hexodoku" hexodoku
    test "Escargot" escargot
    test "Inkala" inkala
    -- test "impossible" impossible
  where
    test label grid = do
        putStrLn label
        case solve grid of
            (n, Nothing) -> printf "Unsolvable after %d cycles\n\n" n
            (n, Just grid') | checkSolved grid' -> printf "Solved after %d cycles\n" n >> print grid'
                            | otherwise -> printf "Invalid solution after %d cycles!\n" n >> print grid'
