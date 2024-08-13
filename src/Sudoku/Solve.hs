{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-} -- For declaring Barbie types
module Sudoku.Solve (Solvable, propagator, PropagatorCmd(..), PropagatorResult(..)) where

import Clash.Prelude hiding (mapAccumR)
import RetroClash.Utils hiding (changed)
import RetroClash.Barbies

import Sudoku.Grid

import Data.Maybe
import Barbies.TH

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

data PropagatorCmd
    = Propagate
    | CommitGuess

data PropagatorResult
    = Solved
    | Failure
    | Guess
    | Progress
    deriving (Generic, NFDataX, Eq, Show)

propagator
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom (Maybe PropagatorCmd)
    -> Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Cell n m)
       , Signal dom PropagatorResult
       , Grid n m (Signal dom (Cell n m))
       )
propagator cmd shift_in pop = (head @(n * m * m * n - 1) (flattenGrid cells), result, conts)
  where
    pops :: Grid n m (Signal dom (Maybe (Cell n m)))
    pops = unbundle . fmap sequenceA $ pop

    units :: Grid n m (Signals dom (CellUnit n m))
    units = pure unit <*> shift_ins <*> prev_guesses <*> pops <*> neighboursMasks masks

    (_shift_out, shift_ins) = shiftInGridAtN (enable (isJust <$> shift_in) . cell <$> units) shift_in
    (guessing_failed, prev_guesses) = shiftInGridAtN (keep_guessing <$> units) (pure True)

    masks = mask <$> units
    cells = cell <$> units
    conts = cont <$> units

    fresh = isJust <$> shift_in .||. isJust <$> pop
    all_unique = bitToBool . reduceAnd <$> bundle (is_unique <$> units)
    any_changed = bitToBool . reduceOr <$> bundle (changed <$> units)
    any_failed  = bitToBool . reduceOr <$> bundle ((.== conflicted) <$> cells)
    can_guess = not <$> all_unique

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
            cmd <- cmd
            guess_this <- guess_this
            guess <- first_guess
            propagated <- propagated
            old <- cell
            pure if
                | Just load <- load                   -> load
                | Just Propagate <- cmd               -> propagated
                | Just CommitGuess <- cmd, guess_this -> guess
                | otherwise                           -> old
        changed = cell' ./=. cell

        (first_guess, next_guess) = unbundle $ splitCell <$> cell
        is_unique = next_guess .== conflicted

        can_guess = not <$> is_unique
        guess_this = try_guess .&&. can_guess
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. is_unique
