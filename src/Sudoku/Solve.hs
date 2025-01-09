{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , consistent

    , solver
    ) where

import Clash.Prelude hiding (mapAccumR)

import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Monoid.Action
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

data Result n m
    = Blocked
    | Complete
    | Progress (Sudoku n m)
    | Stuck (Sudoku n m) (Sudoku n m)
    deriving (Generic, NFDataX)

solve :: (KnownNat n, KnownNat m) => Sudoku n m -> Result n m
solve grid
    | blocked   = Blocked
    | complete  = Complete
    | changed   = Progress pruned
    | otherwise = Stuck first_guess next_guess
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and singles

    (singles, first_guess, next_guess) = expand grid
    pruned = apply <$> group_masks <*> singles <*> grid

    masks = maskOf <$> singles <*> grid
    group_masks = foldGroups masks
    changed = pruned /= grid

    maskOf single cell = if single then cellMask cell else mempty
    apply mask single cell = if single then cell else act mask cell

shiftIn :: (Traversable f) => a -> f a -> f a
shiftIn shift_in = snd . mapAccumR shift shift_in
  where
    shift shift_in cell = (cell, shift_in)

type StackPtr n m = Index (n * m * m * n)
type StackCmd n m = MemCmd (n * m * m * n) (Sudoku n m)

stepSolver
    :: (Solvable n m)
    => Maybe (Sudoku n m)
    -> Result n m
    -> Sudoku n m
    -> StackPtr n m
    -> (Sudoku n m, StackPtr n m, Maybe (StackCmd n m), Maybe Bool)
stepSolver popped result grid sp
    | Just popped <- popped = (popped, sp, Nothing, Nothing)
    | otherwise = case result of
          Blocked
              | sp == 0 -> (grid, sp, Nothing, Just False)
              | otherwise -> let sp' = sp - 1 in (grid, sp', Just (Read sp'), Nothing)
          Complete -> (grid, sp, Nothing, Just True)
          Progress pruned -> (pruned, sp, Nothing, Nothing)
          Stuck first_guess next_guess -> (first_guess, sp + 1, Just (Write sp next_guess), Nothing)

loadOrStepSolver
    :: (Solvable n m)
    => Maybe (Cell n m)
    -> Bool
    -> Maybe (Sudoku n m)
    -> Result n m
    -> Sudoku n m
    -> StackPtr n m
    -> (Sudoku n m, StackPtr n m, Maybe (StackCmd n m), Maybe Bool)
loadOrStepSolver cell_in en popped result grid sp
    | Just cell_in <- cell_in = (shiftIn cell_in grid, 0, Nothing, Nothing)
    | not en = (grid, sp, Nothing, Nothing)
    | otherwise = stepSolver popped result grid sp

solver :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
  => Signal dom (Maybe (Cell n m))
  -> Signal dom Bool
  -> ( Signal dom (Cell n m)
    , Signal dom (Maybe Bool)
    )
solver cell_in en = (cell_out, done)
  where
    grid = register (pure wild) grid'
    sp = register 0 sp'
    cell_out = headGrid <$> grid

    result = solve <$> grid
    (grid', sp', stack_cmd, done) = unbundle $
        loadOrStepSolver <$> cell_in <*> en <*> popped <*> result <*> grid <*> sp

    popped = ram @(n * m * m * n) stack_cmd

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
