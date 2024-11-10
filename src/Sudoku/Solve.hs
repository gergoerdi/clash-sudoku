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

data CellMemo n m = CellMemo
    { cell :: Cell n m
    , single :: Bool
    , firstGuess, nextGuess :: Cell n m
    }

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
solver cmd shift_in pop = (headGrid cells, result, bundle grid2)
  where
    pops = unbundle . fmap sequenceA $ pop

    blocked = void .||. (not <$> safe)
    void = or <$> bundle ((.==. pure conflicted) <$> cells)
    safe = allGroups consistent <$> bundle masks
    complete = and <$> bundle (fmap single <$> memos)

    cells = pure (regMaybe wild) <*> cells'
    cells' = select <$> shift_ins <*> pops <*> pruneds <*> grid1
      where
        select shift_in_prev pop pruned guess = fmap getAlt . getAp . mconcat . coerce $
            [ pop
            , enable (isJust <$> shift_in) shift_in_prev
            , enable (cmd .==. pure Prune .&&. changed) pruned
            , enable (cmd .==. pure Guess) guess
            ]

    shift_ins = traverseS step shift_in cells
      where
        step cell shift_in = case shift_in of
            Nothing -> (cell, Nothing)
            Just shift_in -> (shift_in, Just cell)

    memos = memo <$> cells

    masks = liftA maskOf <$> memos
    group_masks = propagate masks
    pruneds = liftA2 apply <$> group_masks <*> memos
    changed = or <$> (bundle $ (./=.) <$> pruneds <*> cells)

    guesses = traverseS guess1 (pure False) memos
    grid1 = fmap fst <$> guesses
    grid2 = fmap snd <$> guesses

    result =
        mux blocked   (pure Blocked) $
        mux complete  (pure Complete) $
        mux changed   (pure Progress) $
        pure Stuck

    memo cell = CellMemo <$> cell <*> single <*> first_guess <*> next_guess
      where
        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        single = next_guess .==. pure conflicted

    maskOf CellMemo{..} = if single then cellMask cell else mempty
    apply mask CellMemo{..} = if single then cell else act mask cell

    guess1 :: CellMemo n m -> Bool -> ((Cell n m, Cell n m), Bool)
    guess1 CellMemo{..} guessed_before
        | not guessed_before
        , not single
        = ((firstGuess, nextGuess), True)

        | otherwise
        = ((cell, cell), guessed_before)
