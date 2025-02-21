{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , consistent

    , solver
    , Result(..)
    ) where

import Clash.Prelude hiding (mapAccumR)

import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Traversable (mapAccumR)
import Data.Maybe

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

consistent :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

expand :: (KnownNat n, KnownNat m) => Sudoku n m -> (Grid n m Bool, Sudoku n m, Sudoku n m)
expand = funzip3 . snd . mapAccumR guess False
  where
    guess guessed_before cell
        | not guessed_before && not single
        = (True, (single, first_guess, next_guess))

        | otherwise
        = (guessed_before, (single, cell, cell))
      where
        (first_guess, next_guess) = splitCell cell
        single = next_guess == conflicted

data Step n m
    = Blocked
    | Complete
    | Progress (Sudoku n m)
    | Stuck (Sudoku n m) (Sudoku n m)
    deriving (Generic, NFDataX)

solve :: (KnownNat n, KnownNat m) => Sudoku n m -> Step n m
solve grid
    | blocked   = Blocked
    | complete  = Complete
    | changed   = Progress pruned
    | otherwise = Stuck guess cont
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and singles

    (singles, guess, cont) = expand grid
    pruned = act <$> group_masks <*> singles <*> grid

    masks = cellMask <$> singles <*> grid
    group_masks = foldGroups masks
    changed = pruned /= grid

shiftIn :: (Traversable f) => a -> f a -> f a
shiftIn shift_in = snd . mapAccumR shift shift_in
  where
    shift shift_in cell = (cell, shift_in)

type StackPtr n m = Index (n * m * m * n)
type StackCmd n m = MemCmd (n * m * m * n) (Sudoku n m)

data Result
    = Solved
    | Unsolvable
    deriving (Generic, NFDataX, Show)

data Transition s r
    = Continue s
    | Done r

step
    :: (Solvable n m)
    => Maybe (Sudoku n m)
    -> Sudoku n m
    -> StackPtr n m
    -> Transition (Sudoku n m, StackPtr n m, Maybe (StackCmd n m)) Result
step stack_rd grid sp = case solve grid of
    _ | Just popped <- stack_rd -> Continue (popped, sp, Nothing)
    Blocked
        | sp == 0 -> Done Unsolvable
        | otherwise -> Continue (grid, sp', Just (Read sp'))
      where
        sp' = sp - 1
    Complete -> Done Solved
    Progress pruned -> Continue (pruned, sp, Nothing)
    Stuck guess cont -> Continue (guess, sp + 1, Just (Write sp cont))

loadOrStep
    :: (Solvable n m)
    => Maybe (Cell n m)
    -> Bool
    -> Maybe (Sudoku n m)
    -> Sudoku n m
    -> StackPtr n m
    -> Transition (Sudoku n m, StackPtr n m, Maybe (StackCmd n m)) Result
loadOrStep cell_in en stack_rd grid sp
    | Just cell_in <- cell_in = Continue (shiftIn cell_in grid, 0, Nothing)
    | not en = Continue (grid, sp, Nothing)
    | otherwise = step stack_rd grid sp

solver :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom (Maybe (Cell n m))
    -> Signal dom Bool
    -> ( Signal dom (Cell n m)
      , Signal dom (Maybe Result)
      )
solver cell_in en = (cell_out, result)
  where
    grid = register (pure wild) grid'
    sp = register 0 sp'
    cell_out = headGrid <$> grid
    stack_rd = ram stack_cmd

    (grid', sp', stack_cmd, result) = unbundle $ update <$> cell_in <*> en <*> stack_rd <*> grid <*> sp

    update cell_in en stack_rd grid sp = case loadOrStep cell_in en stack_rd grid sp of
        Continue (grid', sp', stack_cmd) -> (grid', sp', stack_cmd, Nothing)
        Done result -> (grid, sp, Nothing, Just result)

data MemCmd n a = Write (Index n) a | Read (Index n)

ram
    :: forall n dom a. (HiddenClockResetEnable dom, KnownNat n, 1 <= n, NFDataX a)
    => Signal dom (Maybe (MemCmd n a))
    -> Signal dom (Maybe a)
ram cmd = enable enable_rd rd
  where
    (rd_addr, wr) = unbundle $ interpret <$> cmd
    enable_rd = delay False $ isJust <$> rd_addr
    rd = blockRamU NoClearOnReset (SNat @n) undefined (fromMaybe undefined <$> rd_addr) wr

    interpret = \case
        Nothing -> (Nothing, Nothing)
        Just (Read addr) -> (Just addr, Nothing)
        Just (Write addr x) -> (Nothing, Just (addr, x))
