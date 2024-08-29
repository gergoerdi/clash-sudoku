{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ApplicativeDo, NumericUnderscores, TupleSections #-}
{-# LANGUAGE UndecidableInstances, ViewPatterns, BangPatterns #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
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

import Sudoku.Matrix
import Sudoku.Grid
import Format
import Sudoku
import Sudoku.Solve (Solvable)
import Text.Printf

model_encodeSerial :: Int -> Word8 -> [Bit]
model_encodeSerial stretch x = mconcat
    [ pause
    , startBit
    , mconcat dataBits
    , stopBit
    ]
  where
    slow = L.replicate stretch
    pause = slow high
    startBit = slow low
    dataBits = L.map (slow . lsb) $ L.take 8 . L.iterate (`shiftR` 1) $ x
    stopBit = slow high

model_encodeSerials :: Int -> [Word8] -> [Bit]
model_encodeSerials stretch xs = (<> L.repeat high) $
    L.concatMap (\x -> model_encodeSerial stretch x <> L.replicate (10 * 1 * stretch) high) xs

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

sim_board :: forall n m. (Solvable n m, Showable n m) => Sudoku n m -> String
sim_board =
    fmap (chr . fromIntegral) .
    simulateCSE @System (exposeClockResetEnable (board (SNat @n) (SNat @m))) .
    fmap ascii . showGrid

sim_topEntity :: Sudoku 3 3 -> String
sim_topEntity =
    fmap (chr . fromIntegral) . model_decodeSerial 16 .
    simulate @System (hideClockResetEnable $ \clk rst _en -> topEntity clk rst) .
    model_encodeSerials 16 . fmap ascii . showGrid @3 @3

readGrid :: forall n m. (Readable n m) => String -> Maybe (Sudoku n m)
readGrid = go []
  where
    go xs cs
        | Just cells <- V.fromList xs
        = Just $ unflattenGrid . reverse $ cells

        | (c:cs) <- cs
        = go (maybe xs (:xs) $ parseCell . ascii $ c) cs

        | [] <- cs
        = Nothing

grid1 :: Sudoku 3 3
Just grid1 = readGrid . unlines $
    [ "0 2 0  9 0 8  0 0 0"
    , "8 7 0  0 0 1  0 5 4"
    , "5 0 6  4 0 0  0 1 0"
    , ""
    , "0 0 2  0 0 0  0 9 5"
    , "0 0 0  0 0 0  0 0 0"
    , "9 4 0  0 0 0  8 0 0"
    , ""
    , "0 8 0  0 0 4  5 0 3"
    , "1 3 0  2 0 0  0 8 6"
    , "0 0 0  3 0 7  0 2 0"
    ]

grid2 :: Sudoku 3 3
Just grid2 = readGrid . unlines $
    [ "0 0 0  6 0 0  5 0 0"
    , "0 6 0  0 0 8  0 4 0"
    , "0 0 0  7 0 4  0 0 0"
    , ""
    , "0 0 2  8 0 0  0 0 9"
    , "0 9 0  0 0 0  0 7 0"
    , "8 0 0  0 0 9  1 0 0"
    , ""
    , "0 0 0  2 0 6  0 0 0"
    , "0 0 0  5 0 0  0 1 0"
    , "0 5 1  0 0 7  0 0 0"
    ]

unsolvable :: Sudoku 3 3
Just unsolvable = readGrid . unlines $
    [ "0 0 0  6 0 0  5 0 0"
    , "0 6 0  0 0 8  0 4 0"
    , "0 0 0  7 0 4  0 0 0"
    , ""
    , "0 0 2  8 0 0  0 0 9"
    , "0 9 0  0 0 0  0 7 0"
    , "8 0 0  0 0 9  1 0 0"
    , ""
    , "0 0 0  2 0 6  0 0 0"
    , "0 3 0  5 0 0  0 1 0"
    , "9 5 1  0 0 7  0 0 0"
    ]

-- | From https://norvig.com/sudoku.html
hard :: Sudoku 3 3
Just hard = readGrid . unlines $
    [ ". . . |. . 6 |. . . "
    , ". 5 9 |. . . |. . 8 "
    , "2 . . |. . 8 |. . . "
    , "------+------+------"
    , ". 4 5 |. . . |. . . "
    , ". . 3 |. . . |. . . "
    , ". . 6 |. . 3 |. 5 4 "
    , "------+------+------"
    , ". . . |3 2 5 |. . 6 "
    , ". . . |. . . |. . . "
    , ". . . |. . . |. . . "
    ]

-- | From https://norvig.com/sudoku.html
impossible :: Sudoku 3 3
Just impossible = readGrid . unlines $
    [ ". . . |. . 5 |. 8 . "
    , ". . . |6 . 1 |. 4 3 "
    , ". . . |. . . |. . . "
    , "------+------+------"
    , ". 1 . |5 . . |. . . "
    , ". . . |1 . 6 |. . . "
    , "3 . . |. . . |. . 5 "
    , "------+------+------"
    , "5 3 . |. . . |. 6 1 "
    , ". . . |. . . |. . 4 "
    , ". . . |. . . |. . . "
    ]

-- | From https://www.sudokuwiki.org/Escargot
escargot :: Sudoku 3 3
Just escargot = readGrid . unlines $
  [ "1 . . | . . 7 | . 9 ."
  , ". 3 . | . 2 . | . . 8"
  , ". . 9 | 6 . . | 5 . ."
  , "------+-------+------"
  , ". . 5 | 3 . . | 9 . ."
  , ". 1 . | . 8 . | . . 2"
  , "6 . . | . . 4 | . . ."
  , "------+-------+------"
  , "3 . . | . . . | . 1 ."
  , ". 4 1 | . . . | . . 7"
  , ". . 7 | . . . | 3 . ."
  ]

-- | From https://sudoku2.com/sudoku-tips/The-hardest-sudoku-in-the-world/
inkala :: Sudoku 3 3
Just inkala = readGrid . unlines $
  [ "8 . . | . . . | . . ."
  , ". . 3 | 6 . . | . . ."
  , ". 7 . | . 9 . | 2 . ."
  , "------+-------+------"
  , ". 5 . | . . 7 | . . ."
  , ". . . | . 4 5 | 7 . ."
  , ". . . | 1 . . | . 3 ."
  , "------+-------+------"
  , ". . 1 | . . . | . 6 8"
  , ". . 8 | 5 . . | . 1 ."
  , ". 9 . | . . . | 4 . ."
  ]

instance (Showable n m) => ShowX (Cell n m)

hexodoku :: Sudoku 4 4
Just hexodoku = readGrid . unlines $
    [ ". 6 . . | E . . .  | . . . 1  | . . 3 ."
    , "9 . . . | . B 4 .  | . F E .  | . . . 7"
    , "A F . . | . G . .  | . . 5 .  | . . 6 E"
    , "E G 2 . | 8 C 5 .  | . 7 A B  | . F 1 4"
    , "--------+----------+----------+--------"
    , "6 A C 2 | . . . .  | . . . .  | 3 5 D 1"
    , "8 1 3 7 | G A . .  | . . D 6  | 2 B 4 9"
    , "B 5 F 4 | D 1 . .  | . . 7 A  | E 6 G C"
    , "D 9 E G | 3 4 6 .  | . C 1 2  | 7 8 A F"
    , "--------+----------+----------+--------"
    , "C B 1 E | A 6 8 .  | . G F 7  | 9 3 2 5"
    , "2 3 D F | B 9 . .  | . . 8 5  | 6 4 E G"
    , "G 7 5 9 | F 2 . .  | . . C 4  | 1 A B 8"
    , "4 8 A 6 | . . . .  | . . . .  | F 7 C D"
    , "--------+----------+----------+--------"
    , "F E G . | C 3 9 .  | . 5 4 D  | . 1 8 6"
    , "1 C . . | . 8 . .  | . . 2 .  | . . 7 3"
    , "5 . . . | . F E .  | . A B .  | . . . 2"
    , ". 2 . . | 7 . . .  | . . . C  | . . F ."
    ]

solve :: forall n m. (Solvable n m, Showable n m) => Sudoku n m -> (Int, Maybe (Sudoku n m))
solve = start (signalAutomaton @System $ bundle . uncurry (controller' @n @m) . unbundle) . toList . flattenGrid
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
checkSolved grid = and
    [ fold @(n * m - 1) (&&) (rowmap valid grid)
    , fold @(n * m - 1) (&&) (colmap valid grid)
    , fold @(n * m - 1) (&&) (toRowMajorOrder $ boxmap valid grid)
    ]
  where
    valid xs = L.sort (toList xs) == [ unique i | i <- [minBound..maxBound] ]

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
