{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , bitsOverlap
    , consistent

    , solver
    , SolverCmd(..)
    , SolverResult(..)
    ) where

import Clash.Prelude hiding (mapAccumR)
import Clash.Num.Overflowing

import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Monoid.Action
import Data.Traversable (mapAccumR)

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

consistent :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

bitsOverlap :: (KnownNat n, KnownNat k) => Vec k (BitVector n) -> Bool
bitsOverlap = any (hasOverflowed . sum . map toOverflowing) . transpose . map bv2v

data SolverCmd
    = Idle
    | Prune
    | Guess
    deriving (Generic, NFDataX, Eq, Show)

data SolverResult
    = Blocked
    | Complete
    | Progress
    | Stuck
    deriving (Generic, NFDataX, Eq, Show)

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

solve
    :: forall n m. (KnownNat n, KnownNat m)
    => SolverCmd
    -> Maybe (Cell n m)
    -> Maybe (Sudoku n m)
    -> Sudoku n m
    -> (SolverResult, Sudoku n m, Sudoku n m)
solve cmd shift_in pop grid = (result, grid', next_guess)
  where
    result
        | blocked   = Blocked
        | complete  = Complete
        | changed   = Progress
        | otherwise = Stuck

    grid'
        | Just pop <- pop       = pop
        | Just _ <- shift_in    = shifted_in
        | Prune <- cmd, changed = pruned
        | Guess <- cmd          = first_guess
        | otherwise            = grid

    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and singles

    shifted_in = snd $ mapAccumR step shift_in grid
      where
        step shift_in cell = case shift_in of
            Nothing -> (Nothing, cell)
            Just shift_in -> (Just cell, shift_in)

    (singles, first_guess, next_guess) = expand grid
    pruned = apply <$> group_masks <*> singles <*> grid

    masks = maskOf <$> singles <*> grid
    group_masks = foldGroups masks
    changed = pruned /= grid

    maskOf single = if single then cellMask else mempty
    apply mask single = if single then id else act mask 

solver
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom SolverCmd
    -> Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom SolverResult
       , Signal dom (Cell n m)
       , Signal dom (Sudoku n m)
       )
solver cmd shift_in pop = (result, headGrid cells, next_guess)
  where
    cells = pure (register wild) <*> cells'
    cells' = unbundle grid'

    grid = bundle cells
    (result, grid', next_guess) = unbundle $ solve <$> cmd <*> shift_in <*> pop <*> grid
