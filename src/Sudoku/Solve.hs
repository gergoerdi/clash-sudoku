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

import Data.Maybe
import Data.Monoid.Action
import Data.Monoid (Ap(..), Alt(..))
import Data.Coerce
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

propagate
    :: (Solvable n m, HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Mask n m))
    -> Grid n m (Signal dom (Mask n m))
propagate = fmap getAp . foldGroups . fmap Ap

funzip3 :: (Functor f) => f (a, b, c) -> (f a, f b, f c)
funzip3 xyzs = ((\(x, y, z) -> x) <$> xyzs, (\(x, y, z) -> y) <$> xyzs, (\(x, y, z) -> z) <$> xyzs)

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

    (_, shifted_ins) = mapAccumRS step shift_in cells
      where
        step shift_in cell = case shift_in of
            Nothing -> (Nothing, cell)
            Just shift_in -> (Just cell, shift_in)

    pruneds = apply .<$>. group_masks .<*>. singles .<*>. cells

    (unbundle -> singles, unbundle -> first_guesses, unbundle -> next_guesses) = unbundle . fmap expand . bundle $ cells

    masks = maskOf .<$>. singles .<*>. cells
    group_masks = propagate masks
    changed = or <$> (bundle $ (/=) .<$>. pruneds .<*>. cells)

    maskOf single cell = if single then cellMask cell else mempty
    apply mask single cell = if single then cell else act mask cell
