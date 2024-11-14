{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, LambdaCase #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , bitsOverlap
    , consistent

    , solver
    , SolverCmd(..)
    , Result(..)
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
    deriving (Eq)

data Result a
    = Blocked
    | Complete
    | Progress a
    | Stuck a
    deriving (Generic, NFDataX, Functor, Foldable, Traversable)

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

solve :: (KnownNat n, KnownNat m) => Sudoku n m -> (Result (Sudoku n m), Sudoku n m)
solve grid = (result, next_guess)
  where
    result
      | blocked   = Blocked
      | complete  = Complete
      | changed   = Progress pruned
      | otherwise = Stuck first_guess

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

shiftIn :: (KnownNat n, KnownNat m) => Maybe a -> Grid n m a -> Grid n m (Maybe a)
shiftIn shift_in = snd . mapAccumR shift shift_in
  where
    shift shift_in cell = case shift_in of
        Nothing -> (Nothing, Nothing)
        Just shift_in -> (Just cell, Just shift_in)

commit :: (KnownNat n, KnownNat m) => SolverCmd -> Result (Cell n m) -> Maybe (Cell n m) -> Maybe (Cell n m) -> Maybe (Cell n m)
commit cmd result pop shifted
    | Just pop <- pop
    = Just pop

    | Just shifted <- shifted
    = Just shifted

    | Prune <- cmd, Progress pruned <- result
    = Just pruned

    | Guess <- cmd, Stuck first_guess <- result
    = Just first_guess

    | otherwise
    = Nothing

commit'
    :: (KnownNat n, KnownNat m)
    => SolverCmd
    -> Result (Sudoku n m)
    -> Maybe (Sudoku n m)
    -> Grid n m (Maybe (Cell n m))
    -> Grid n m (Maybe (Cell n m))
commit' cmd result pop shifted = commit cmd <$> sequenceA result <*> sequenceA pop <*> shifted

solver
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom SolverCmd
    -> Signal dom (Maybe (Sudoku n m))
    -> Signal dom (Maybe (Cell n m))
    -> ( Signal dom (Result (Sudoku n m))
       , Signal dom (Cell n m)
       , Signal dom (Sudoku n m)
       )
solver cmd pop shift_in = (result, headGrid cells, next_guess)
  where
    cells = pure (regMaybe wild) <*> cells'

    grid = bundle cells
    (result, next_guess) = unbundle $ solve <$> grid

    cells' = unbundle $ commit' <$> cmd <*> result <*> pop <*> shifted
    shifted = shiftIn <$> shift_in <*> grid
