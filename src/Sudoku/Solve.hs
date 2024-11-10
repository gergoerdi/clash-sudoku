{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , bitsOverlap
    , consistent

    , propagator
    , PropagatorCmd(..)
    , PropagatorResult(..)
    ) where

import Clash.Prelude
import Clash.Num.Overflowing

import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Maybe
import Data.Monoid.Action
import Control.Monad.State.Strict
import Data.Monoid (Ap(..), Alt(..))
import Data.Coerce

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

consistent :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

bitsOverlap :: (KnownNat n, KnownNat k) => Vec k (BitVector n) -> Bool
bitsOverlap = any (hasOverflowed . sum . map toOverflowing) . transpose . map bv2v

data CellMemo dom n m = CellMemo
    { cell :: Signal dom (Cell n m)
    , single :: Signal dom Bool
    , firstGuess, nextGuess :: Signal dom (Cell n m)
    }

data PropagatorCmd
    = Propagate
    | CommitGuess
    deriving (Generic, NFDataX, Eq, Show)

data PropagatorResult
    = Progress
    | Solved
    | Failure
    | Stuck
    deriving (Generic, NFDataX, Eq, Show)

propagate
    :: (Solvable n m, HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Mask n m))
    -> Grid n m (Signal dom (Mask n m))
propagate = fmap getAp . foldGroups . fmap Ap

propagator
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom (Maybe PropagatorCmd)
    -> Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Cell n m)
       , Signal dom PropagatorResult
       , Signal dom (Sudoku n m)
       )
propagator cmd shift_in pop = (headGrid cells, result, bundle grid2)
  where
    pops = unbundle . fmap sequenceA $ pop

    blocked = void .||. (not <$> safe)
    void = or <$> bundle ((.==. pure conflicted) <$> cells)
    safe = allGroups consistent <$> bundle masks
    complete = and <$> bundle (single <$> memos)

    cells = pure (regMaybe wild) <*> cells'
    cells' = select <$> shift_ins <*> pops <*> pruneds <*> grid1
      where
        select shift_in_prev pop pruned guess = fmap getAlt . getAp . mconcat . coerce $
            [ pop
            , enable (isJust <$> shift_in) shift_in_prev
            , enable (cmd .==. pure (Just Propagate) .&&. changed) pruned
            , enable (cmd .==. pure (Just CommitGuess)) guess
            ]

    shift_ins = traverseS step shift_in cells
      where
        step cell shift_in = case shift_in of
            Nothing -> (cell, Nothing)
            Just shift_in -> (shift_in, Just cell)

    memos = memo <$> cells
    masks = maskOf <$> memos
    group_masks = propagate masks

    pruneds = apply <$> group_masks <*> memos
    changed = or <$> (bundle $ (./=.) <$> pruneds <*> cells)

    result =
        mux blocked   (pure Failure) $
        mux complete  (pure Solved) $
        mux changed   (pure Progress) $
        pure Stuck


    memo cell = CellMemo{..}
      where
        (firstGuess, nextGuess) = unbundle $ splitCell <$> cell
        single = nextGuess .==. pure conflicted

    maskOf CellMemo{..} = mux single (cellMask <$> cell) (pure mempty)
    apply mask CellMemo{..} = mux single cell (act <$> mask <*> cell)

    guesses = evalState (traverse (state . guess1) memos) (pure False)
    grid1 = fmap fst <$> guesses
    grid2 = fmap snd <$> guesses

    guess1 :: CellMemo dom n m -> Signal dom Bool -> (Signal dom (Cell n m, Cell n m), Signal dom Bool)
    guess1 CellMemo{..} guessed_before = unbundle $
        mux (not <$> guessed_before .&&. not <$> single)
          (bundle (bundle (firstGuess, nextGuess), pure True))
          (bundle (bundle (cell, cell), guessed_before))
