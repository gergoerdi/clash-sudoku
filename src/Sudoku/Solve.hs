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
import Data.Monoid (Ap(..))

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

consistent :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

bitsOverlap :: (KnownNat n, KnownNat k) => Vec k (BitVector n) -> Bool
bitsOverlap = any (hasOverflowed . sum . map toOverflowing) . transpose . map bv2v

data CellUnit dom n m = CellUnit
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
    complete = and <$> bundle (single <$> units)

    cells :: Grid n m (Signal dom (Cell n m))
    cells = pure (regMaybe wild) <*> cells'

    shift_ins :: Grid n m (Signal dom (Cell n m))
    shift_ins = evalState (traverse (state . fmap unbundle . liftA2 step) cells) shift_in
      where
        step cell shift_in = case shift_in of
            Nothing -> (cell, Nothing)
            Just shift_in -> (shift_in, Just cell)

    cells' :: Grid n m (Signal dom (Maybe (Cell n m)))
    cells' = unbundle $ fmap sequenceA $
              enable (isJust <$> shift_in) (bundle shift_ins)
        .<|>. pop
        .<|>. enable (cmd .==. pure (Just Propagate) .&&. changed) (bundle pruned)
        .<|>. enable (cmd .==. pure (Just CommitGuess)) (bundle grid1)

    units :: Grid n m (CellUnit dom n m)
    units = unit <$> cells
    masks = maskOf <$> units
    group_masks = propagate masks

    pruned :: Grid n m (Signal dom (Cell n m))
    pruned = apply <$> group_masks <*> units

    changed :: Signal dom Bool
    changed = or <$> (bundle $ (./=.) <$> pruned <*> cells)

    result =
        mux blocked  (pure Failure) $
        mux complete (pure Solved) $
        mux changed  (pure Progress) $
        pure Stuck


    unit :: Signal dom (Cell n m) -> CellUnit dom n m
    unit cell = CellUnit{..}
      where
        (firstGuess, nextGuess) = unbundle $ splitCell <$> cell
        single = nextGuess .==. pure conflicted

    maskOf CellUnit{..} = mux single (cellMask <$> cell) (pure mempty)
    apply mask CellUnit{..} = mux single cell (act <$> mask <*> cell)

    guesses = evalState (traverse (state . guess1) units) (pure False)
    grid1 = fmap fst <$> guesses
    grid2 = fmap snd <$> guesses

    guess1 :: CellUnit dom n m -> Signal dom Bool -> (Signal dom (Cell n m, Cell n m), Signal dom Bool)
    guess1 CellUnit{..} guessed_before = unbundle $
        mux (not <$> guessed_before .&&. not <$> single)
          (bundle (bundle (firstGuess, nextGuess), pure True))
          (bundle (bundle (cell, cell), guessed_before))
