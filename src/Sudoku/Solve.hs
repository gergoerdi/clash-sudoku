{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-} -- For declaring Barbie types
module Sudoku.Solve (Solvable, propagator, PropagatorResult(..)) where

import Clash.Prelude hiding (mapAccumR)
import Clash.Class.Counter
import RetroClash.Utils hiding (changed)
import RetroClash.Barbies

import Sudoku.Hacks
import Sudoku.Matrix
import Sudoku.Grid
import Sudoku.Stack

import Control.Arrow (second, (***))
import Data.Maybe
import Barbies
import Barbies.Bare
import Barbies.TH

import Debug.Trace

foldGrid :: forall n m a. (1 <= n * m * m * n) => (a -> a -> a) -> Grid n m a -> a
foldGrid f = fold @(n * m * m * n - 1) f . flattenGrid

shiftInGridAtN :: forall n m a. (KnownNat n, KnownNat m) => Grid n m a -> a -> (a, Grid n m a)
shiftInGridAtN grid x = (x', unflattenGrid grid')
  where
    (grid', x' :> Nil) = shiftInAtN (flattenGrid grid) (x :> Nil)

neighboursMasks
    :: forall n m dom. (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 2 <= n * m)
    => (HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Mask n m))
    -> Grid n m (Signal dom (Mask n m))
neighboursMasks masks = combine <$> rowwise others masks <*> columnwise others masks <*> boxwise others masks
  where
    combine ms1 ms2 ms3 = fold @(3 * (n * m - 1) - 1) (liftA2 (<>)) (ms1 ++ ms2 ++ ms3)

declareBareB [d|
  data CellUnit n m = CellUnit
    { cell :: Cell n m
    , mask :: Mask n m
    , is_unique :: Bool
    , changed :: Bool
    , cont :: Cell n m
    , keep_guessing :: Bool
    } |]

type Solvable n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 2 <= n * m, 1 <= n * m * m * n)

data PropagatorResult
    = Solved
    | Failure
    | Guess
    | Progress
    deriving (Generic, NFDataX, Eq, Show)

propagator
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Cell n m)
       , Signal dom PropagatorResult
       , Grid n m (Signal dom (Cell n m))
       , Signal dom Bool
       , Grid n m (Signal dom (Cell n m))
       )
propagator enable_propagate commit_guess shift_in pop = (head @(n * m * m * n - 1) (flattenGrid cells), result, cells, can_guess, conts)
  where
    pops :: Grid n m (Signal dom (Maybe (Cell n m)))
    pops = unbundle . fmap sequenceA $ pop

    units :: Grid n m (Signals dom (CellUnit n m))
    units = pure unit <*> shift_ins <*> prev_guesses <*> pops <*> neighboursMasks masks

    (_shift_out, shift_ins) = shiftInGridAtN (enable (isJust <$> shift_in) . cell <$> units) shift_in
    (guessing_failed, prev_guesses) = shiftInGridAtN (keep_guessing <$> units) should_guess

    masks = mask <$> units
    cells = cell <$> units
    conts = cont <$> units

    should_guess = not <$> any_changed
    can_guess = should_guess .&&. (not <$> guessing_failed)

    fresh = isJust <$> shift_in .||. isJust <$> pop
    all_unique = foldGrid (.&&.) (is_unique <$> units)
    any_changed = register False $ foldGrid (.||.) (changed <$> units)
    any_failed  = foldGrid (.||.) ((.== conflicted) <$> cells)

    result =
        mux fresh (pure Progress) $
        mux any_failed (pure Failure) $
        mux all_unique (pure Solved) $
        mux any_changed (pure Progress) $
        mux can_guess (pure Guess) $
        pure Failure

    unit
        :: Signal dom (Maybe (Cell n m))
        -> Signal dom Bool
        -> Signal dom (Maybe (Cell n m))
        -> Signal dom (Mask n m)
        -> Signals dom (CellUnit n m)
    unit shift_in try_guess pop neighbours_mask = CellUnit{..}
      where
        load = shift_in .<|>. pop
        shift_out = enable (isJust <$> shift_in) cell

        cell = register conflicted cell'
        mask = mux is_unique (cellMask <$> cell) (pure mempty)
        propagated = applyMask <$> neighbours_mask <*> cell

        cell' = do
            load <- load
            can_guess <- commit_guess .&&. guess_this
            can_propagate <- enable_propagate
            guess <- first_guess
            propagated <- propagated
            old <- cell
            pure if
                | Just load <- load -> load
                | can_guess        -> guess
                | can_propagate    -> propagated
                | otherwise        -> old
        changed = cell' ./=. cell

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_unique = next_guess .== conflicted

        can_guess = not <$> is_unique
        guess_this = enable_propagate .&&. try_guess .&&. can_guess
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. (not <$> guess_this)
