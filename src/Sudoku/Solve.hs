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

neighbourMasks
    :: forall n m dom. (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 1 <= n * m)
    => (HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Mask n m))
    -> Grid n m (Vec (3 * (n * m - 1)) (Signal dom (Mask n m)))
neighbourMasks masks =
    rowwise others masks .++. columnwise others masks .++. boxwise others masks
  where
    (.++.) = liftA2 (++)
    infixr 5 .++.

propagate
    :: forall n m dom k. (KnownNat n, KnownNat m, KnownNat k)
    => (HiddenClockResetEnable dom)
    => Signal dom (Cell n m)
    -> Vec k (Signal dom (Mask n m))
    -> Signal dom (Cell n m)
propagate cell neighbour_masks = applyMasks <$> cell <*> bundle neighbour_masks

declareBareB [d|
  data CellUnit n m = CellUnit
    { cell :: Cell n m
    , mask :: Mask n m
    , is_unique :: Bool
    , changed :: Bool
    , cont :: Cell n m
    } |]

type Solvable n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= n * m * m * n)

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
    ((_shift_out, keep_guessing), unflattenGrid -> units) =
        mapAccumR unit (shift_in, should_guess) (flattenGrid $ ((,) <$> pops <*> neighbourMasks masks))

    masks = mask <$> units
    cells = cell <$> units
    conts = cont <$> units

    should_guess = not <$> any_changed
    can_guess = should_guess .&&. (not <$> keep_guessing)

    fresh = isJust <$> shift_in .||. isJust <$> pop
    all_unique = foldGrid (.&&.) (is_unique <$> units)
    any_changed = foldGrid (.||.) (changed <$> units)
    any_failed  = foldGrid (.||.) ((.== conflicted) <$> cells)

    result =
        mux fresh (pure Progress) $
        mux any_failed (pure Failure) $
        mux all_unique (pure Solved) $
        mux any_changed (pure Progress) $
        mux can_guess (pure Guess) $
        pure Failure

    unit
        :: forall k. (KnownNat k)
        => (Signal dom (Maybe (Cell n m)), Signal dom Bool)
        -> (Signal dom (Maybe (Cell n m)), Vec k (Signal dom (Mask n m)))
        -> ((Signal dom (Maybe (Cell n m)), Signal dom Bool), Signals dom (CellUnit n m))
    unit (shift_in, try_guess) (pop, neighbour_masks) = ((shift_out, keep_guessing), CellUnit{..})
      where
        load = shift_in .<|>. pop
        shift_out = enable (isJust <$> shift_in) cell

        cell = register conflicted cell'
        mask = mux is_unique (cellMask <$> cell) (pure wildMask)

        -- TODO: why do we need to delay this?
        changed = register False changed_
        (cell', changed_) = unbundle do
            load <- load
            guess <- enable (commit_guess .&&. guess_this) first_guess
            propagate <- enable enable_propagate $ propagate cell neighbour_masks
            old <- cell
            pure $ let new = fromMaybe old $ load <|> guess <|> propagate in (new, new /= old)

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_unique = next_guess .== conflicted

        can_guess = not <$> is_unique
        guess_this = enable_propagate .&&. try_guess .&&. can_guess
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. (not <$> guess_this)
