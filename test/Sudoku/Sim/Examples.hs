{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ApplicativeDo, NumericUnderscores, TupleSections #-}
{-# LANGUAGE UndecidableInstances, ViewPatterns, BangPatterns #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}
module Sudoku.Sim.Examples where

import Clash.Prelude hiding (lift)
import qualified Clash.Sized.Vector as V

import Sudoku.Grid
import Sudoku.Solve (Sudoku)
import Sudoku.Cell
import Format (ascii)

readGrid :: forall n m. (Textual n m) => String -> Maybe (Sudoku n m)
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

instance (Textual n m) => ShowX (Cell n m)

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
