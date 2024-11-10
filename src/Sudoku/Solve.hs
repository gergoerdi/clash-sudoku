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

import Clash.Prelude
import Clash.Num.Overflowing

import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid.Action
import Data.Monoid (Ap(..), Alt(..))
import Data.Coerce

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

propagate
    :: (Solvable n m, HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Mask n m))
    -> Grid n m (Signal dom (Mask n m))
propagate = fmap getAp . foldGroups . fmap Ap

solver
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom SolverCmd
    -> Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Cell n m)
       , Signal dom SolverResult
       , Signal dom (Sudoku n m)
       )
solver cmd shift_in pop = (headGrid cells, result, bundle next_guesses)
  where
    pops = unbundle . fmap sequenceA $ pop

    result =
        mux (void .||. not <$> safe) (pure Blocked) $
        mux (and <$> bundle singles) (pure Complete) $
        mux changed                  (pure Progress) $
        pure Stuck
      where
        void = or <$> bundle ((== conflicted) .<$>. cells)
        safe = allGroups consistent <$> bundle masks

    cells = pure (regMaybe wild) <*> cells'
    cells' = select <$> shifted_ins <*> pops <*> pruneds <*> first_guesses
      where
        select shifted_in pop pruned guess = fmap getAlt . getAp . mconcat . coerce $
            [ pop
            , enable (isJust <$> shift_in) shifted_in
            , enable (cmd .==. pure Prune .&&. changed) pruned
            , enable (cmd .==. pure Guess) guess
            ]

    shifted_ins = traverseS step shift_in cells
      where
        step cell shift_in = case shift_in of
            Nothing -> (cell, Nothing)
            Just shift_in -> (shift_in, Just cell)

    pruneds = apply .<$>. group_masks .<*>. singles .<*>. cells

    splits = splitCell .<$>. cells
    singles = (== conflicted) . snd .<$>. splits

    masks = maskOf .<$>. singles .<*>. cells
    group_masks = propagate masks
    changed = or <$> (bundle $ (/=) .<$>. pruneds .<*>. cells)

    maskOf single cell = if single then cellMask cell else mempty
    apply mask single cell = if single then cell else act mask cell

    guesses = traverseS guess1 (pure False) ((,,) .<$>. singles .<*>. cells .<*>. splits)
    first_guesses = fst .<$>. guesses
    next_guesses = snd .<$>. guesses

    guess1 (single, cell, guess) guessed_before
        | not guessed_before
        , not single
        = (guess, True)

        | otherwise
        = ((cell, cell), guessed_before)
