{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-} -- For declaring Barbie types
module Sudoku.Solve (propagator, PropagatorResult(..)) where

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

foldGrid :: (n * m * m * n ~ k + 1) => (a -> a -> a) -> Grid n m a -> a
foldGrid f = fold f . flattenGrid

unzipMatrix :: Matrix n m (a, b) -> (Matrix n m a, Matrix n m b)
unzipMatrix = (FromRows *** FromRows) . unzip . fmap unzip . matrixRows

unzipGrid :: Grid n m (a, b) -> (Grid n m a, Grid n m b)
unzipGrid = (Grid *** Grid) . unzipMatrix . fmap unzipMatrix . getGrid

neighbourMasks
    :: forall n m dom. (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 1 <= n * m)
    => (HiddenClockResetEnable dom)
    => Grid n m (Signal dom (Cell n m))
    -> Grid n m (Signal dom Bool)
    -> Grid n m (Vec (3 * (n * m - 1)) (Signal dom (Mask n m)))
neighbourMasks cells uniques =
    rowwise others masks .++. columnwise others masks .++. boxwise others masks
  where
    masks = cellMask <$> cells <*> uniques

    cellMask :: Signal dom (Cell n m) -> Signal dom Bool -> Signal dom (Mask n m)
    cellMask c is_unique = mux is_unique (Mask . complement . cellBits <$> c) (pure wildMask)

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
    , is_unique :: Bool
    , changed :: Bool
    , cont :: Cell n m
    } |]

data PropagatorResult
    = Solved
    | Failure
    | Guess
    | Progress
    deriving (Generic, NFDataX, Eq, Show)

propagator
    :: forall n m dom. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= n * m * m * n)
    => (HiddenClockResetEnable dom)
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
propagator enable_propagate commit_guess shift_in pop = (head (flattenGrid cells), result, cells, can_guess, conts)
  where
    pops :: Grid n m (Signal dom (Maybe (Cell n m)))
    pops = unbundle . fmap sequenceA $ pop

    ((_shift_out, keep_guessing), units) =
        mapAccumR unit (shift_in, should_guess) (flattenGrid $ ((,) <$> pops <*> neighbourMasks cells uniques))

    cells = unflattenGrid $ cell <$> units
    uniques = unflattenGrid $ is_unique <$> units
    conts = unflattenGrid $ cont <$> units

    faileds = fmap (.== conflicted) cells

    should_guess = not <$> any_changed
    can_guess = should_guess .&&. (not <$> keep_guessing)

    fresh = register False $ isJust <$> shift_in .||. isJust <$> pop
    all_unique = foldGrid (.&&.) uniques
    any_changed = fold @(n * m * m * n - 1) (.||.) (changed <$> units)
    any_failed  = foldGrid (.||.) faileds

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

        -- TODO: why do we need to delay this?
        changed = register False changed_
        (cell', changed_) = unbundle do
            load <- load
            guess <- enable (commit_guess .&&. guess_this) first_guess
            propagate <- enable enable_propagate $ propagate cell neighbour_masks
            old <- cell
            pure $ let new = fromMaybe old $ load <|> guess <|> propagate in (new, new /= old)
            -- pure $ case load <|> guess of
            --     Just new_value -> (new_value, True)
            --     Nothing -> case propagate of
            --         Just propagate -> (propagate, propagate /= old)
            --         Nothing -> (old, False)

        first_guess = cellFirstBit <$> cell
        next_guess = cellOtherBits <$> cell <*> first_guess
        is_unique = next_guess .== conflicted

        can_guess = not <$> is_unique
        guess_this = enable_propagate .&&. try_guess .&&. can_guess
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. (not <$> guess_this)
