{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , bitsOverlap
    , consistent

    , machine
    , Result(..)
    ) where

import Clash.Prelude
import Clash.Num.Overflowing

import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid.Action
import Control.Monad.State.Strict
import Data.Monoid (Ap(..))

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

consistent :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

bitsOverlap :: (KnownNat n, KnownNat k) => Vec k (BitVector n) -> Bool
bitsOverlap = any (hasOverflowed . sum . map toOverflowing) . transpose . map bv2v

data CellUnit n m = CellUnit
    { cell :: Cell n m
    , single :: Bool
    , first_guess, next_guess :: Cell n m
    }

data Result
    = Blocked
    | Complete
    | Pruned
    | Guess
    deriving (Generic, NFDataX, Show)

solve :: (Solvable n m) => Sudoku n m -> (Result, Sudoku n m, Sudoku n m)
solve grid
    | blocked   = (Blocked, grid, grid2)
    | complete  = (Complete, grid, grid2)
    | changed   = (Pruned, pruned, grid2)
    | otherwise = (Guess, grid1, grid2)
  where
    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = all single units

    consistent = not . bitsOverlap . fmap maskBits

    units = unit <$> grid
    unit cell = CellUnit{..}
      where
        (first_guess, next_guess) = splitCell cell
        single = next_guess == conflicted

    masks = maskOf <$> units
    group_masks = foldGroups masks

    pruned = apply <$> group_masks <*> units
    changed = pruned /= grid

    maskOf CellUnit{..} = if single then cellMask cell else mempty
    apply mask CellUnit{..} = if single then cell else act mask cell

    guesses = evalState (traverse (state . guess1) units) False
    (grid1, grid2) = (fst <$> guesses, snd <$> guesses)

    guess1 CellUnit{..} guessed_before
        | not guessed_before
        , not single
        = ((first_guess, next_guess), True)

        | otherwise
        = ((cell, cell), guessed_before)

machine :: (Solvable n m) => Maybe (Cell n m) -> Maybe (Sudoku n m) -> Bool -> State (Sudoku n m) (Result, Sudoku n m)
machine shift_in pop run
    | Just cell <- shift_in
    = (Pruned, undefined) <$ modify (shiftIn cell)

    | Just pop <- pop
    = (Pruned, undefined) <$ put pop

    | run
    = do
          (result, grid', grid2) <- solve <$> get
          put grid'
          pure (result, grid2)

    | otherwise
    = pure (Pruned, undefined)
