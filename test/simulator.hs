{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns, BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
module Main where

import Clash.Prelude hiding (lift)
import Protocols.Internal (simulateCSE)
import qualified Protocols.Df as Df
import Protocols (Ack(..))

import Data.Char (chr)
import qualified Data.List as L
import qualified Clash.Sized.Vector as V
import Data.Word
import Control.Arrow.Transformer.Automaton
import Data.Proxy

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
    formatModel (Proxy @(GridFormat n m)) .
    fmap showCell .
    toList . flattenGrid

instance (Textual n m) => Show (Sudoku n m) where
    show = showGrid

model_decodeSerial :: Int -> [Bit] -> [Word8]
model_decodeSerial stretch = wait
  where
    wait [] = []
    wait bs@(b:bs')
      | b == low = start bs
      | otherwise = wait bs'

    start bs
      | (bs, bs') <- L.splitAt stretch bs
      = dataBits 8 0 bs'

    dataBits 0 x bs = x : end bs
    dataBits n x bs
      | (bs, bs') <- L.splitAt stretch bs
      , let x' = x `shiftR` 1
            x'' = if L.all (== high) bs then x' `setBit` 7 else x'
      = dataBits (n - 1) x'' bs'

    end bs = wait bs

sim_board :: forall n m. (Solvable n m, Textual n m) => Sudoku n m -> String
sim_board =
    fmap (chr . fromIntegral) .
    simulateCSE @System (exposeClockResetEnable (board (SNat @n) (SNat @m))) .
    fmap ascii . showGrid

sim_topEntity :: Sudoku 3 3 -> String
sim_topEntity =
    fmap (chr . fromIntegral) . model_decodeSerial 16 .
    simulate @System (hideClockResetEnable $ \clk rst _en -> topEntity clk rst) .
    encodeSerials 16 . fmap ascii . showGrid @3 @3

solve :: forall n m. (Solvable n m, Textual n m) => Sudoku n m -> (Int, Maybe (Sudoku n m))
solve = start (signalAutomaton @System $ bundle . controller' @n @m . unbundle) . toList . flattenGrid
  where
    start (Automaton step) xs = load sim xs
      where
        (_, sim) = step (Df.NoData, Ack True)

    load (Automaton step) xs@(x:xs') = load sim $ if ack then xs' else xs
      where
        ((Ack ack, _), sim) = step (Df.Data x, Ack True)
    load sim [] = wait 0 sim

    wait !n (Automaton step) = case output of
        Df.Data{} -> consume n [] sim
        Df.NoData -> wait (n + 1) sim
      where
        ((_, output), sim) = step (Df.NoData, Ack False)

    consume n acc (Automaton step)
        | Just cells <- V.fromList (L.reverse acc') = (n, Just $ unflattenGrid cells)
        | otherwise = consume n acc' sim
      where
        ((_, output), sim) = step (Df.NoData, Ack True)

        acc' = case output of
            Df.Data x -> x : acc
            Df.NoData -> acc

checkSolved :: forall n m. (Solvable n m) => Sudoku n m -> Bool
checkSolved = bitToBool . reduceAnd . fmap getAnd . neighbourhoodwise (And . valid)
  where
    valid xs = L.sort (toList xs) == L.sort [ unique i | i <- [minBound..maxBound] ]

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
