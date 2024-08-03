{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-} -- For declaring Barbie types
{-# LANGUAGE PartialTypeSignatures #-}
module Sudoku.Solve  where

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
import Barbies.TH

rotateL1 :: Vec n a -> Vec n a
rotateL1 Nil = Nil
rotateL1 (Cons x xs) = Cons x' xs'
  where
    (x', xs') = mapAccumR (\next x -> (x, next)) x xs

foldGrid :: forall n m a. (1 <= n * m * m * n) => (a -> a -> a) -> Grid n m a -> a
foldGrid f = fold @(n * m * m * n - 1) f . flattenGrid

shiftInGridAtN :: forall n m a. (KnownNat n, KnownNat m) => Grid n m a -> a -> (a, Grid n m a)
shiftInGridAtN grid x = (x', unflattenGrid grid')
  where
    (grid', x' :> Nil) = shiftInAtN (flattenGrid grid) (x :> Nil)

shiftInGridAt0 :: forall n m a. (KnownNat n, KnownNat m) => a -> Grid n m a -> (Grid n m a, a)
shiftInGridAt0 x grid = (unflattenGrid grid', x')
  where
    (grid', x' :> Nil) = shiftInAt0 (flattenGrid grid) (x :> Nil)

data PropagatorResult
    = Solved
    | Failure
    | Guess
    | Progress
    deriving (Generic, NFDataX, Eq, Show)

declareBareB [d|
  data CellUnit n m = CellUnit
    { cell :: Cell n m
    , is_unique :: Bool
    , changed :: Bool
    , cont :: Cell n m
    , row_neighbour, col_neighbour, box_neighbour :: Mask n m
    , keep_guessing :: Bool
    } |]

type Solvable n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= n * m * m * n)

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
propagator enable_propagate commit_guess shift_in pop = (head_cell, result, cells, can_guess, conts)
  where
    cnt = register (0 :: Index (n * m)) $ do
        result <- result
        enable_propagate <- enable_propagate
        cnt <- cnt
        pure $ case result of
            Progress | enable_propagate -> countSucc cnt
            _ -> cnt
    new_round = cnt .== 0

    pops :: Grid n m (Signal dom (Maybe (Cell n m)))
    pops = unbundle . fmap sequenceA $ pop

    prev_bufs :: Grid n m (Signal dom (Mask n m), Signal dom (Mask n m), Signal dom (Mask n m))
    prev_bufs = (,,)
        <$> rowwise rotateL1 (row_neighbour <$> units)
        <*> columnwise rotateL1 (col_neighbour <$> units)
        <*> boxwise rotateL1 (box_neighbour <$> units)

    units :: Grid n m (Signals dom (CellUnit n m))
    units = pure unit <*> shift_ins <*> guess_laters <*> pops <*> prev_bufs

    cells = cell <$> units
    conts = cont <$> units

    shift_ins :: Grid n m (Signal dom (Maybe (Cell n m)))
    (_shift_out, shift_ins) = shiftInGridAtN (enable (isJust <$> shift_in) <$> cells) shift_in
    head_cell = head @(n * m * m * n - 1) $ flattenGrid cells
    (guess_laters, guessing_failed) = shiftInGridAt0 (pure True) (keep_guessing <$> units)

    should_guess = not <$> any_changed
    can_guess = {- should_guess .&&. -} (not <$> guessing_failed)

    any_changed = foldGrid (.||.) (changed <$> units)
    all_unique = foldGrid (.&&.) (is_unique <$> units)
    any_failed  = foldGrid (.||.) ((.== conflicted) <$> cells)

    unit :: Signal dom (Maybe (Cell n m)) -> Signal dom Bool -> Signal dom (Maybe (Cell n m)) -> (Signal dom (Mask n m), Signal dom (Mask n m), Signal dom (Mask n m)) -> Signals dom (CellUnit n m)
    unit shift_in try_guess pop (prev_row, prev_col, prev_box) = CellUnit{..}
      where
        load = shift_in .<|>. pop

        cell = register conflicted cell'
        is_unique = isUnique <$> cell

        (cell', changed) = unbundle do
            load <- load
            guess <- enable (commit_guess .&&. guess_this) first_guess
            propagate <- enable enable_propagate (applyMask <$> cell <*> mask)
            old <- cell
            pure $ case load <|> guess of
                Just new_value -> (new_value, True)
                Nothing -> case propagate of
                    Just propagate -> (propagate, propagate /= old)
                    Nothing -> (old, False)

        extend_mask mask = extendMask <$> mask <*> is_unique <*> cell

        propagator_buf = register wildMask . mux new_round (pure wildMask)
        row_buf = propagator_buf prev_row
        col_buf = propagator_buf prev_col
        box_buf = propagator_buf prev_box
        mask = combineMask <$> row_buf <*> (combineMask <$> col_buf <*> box_buf)

        row_neighbour = extend_mask row_buf
        col_neighbour = extend_mask col_buf
        box_neighbour = extend_mask box_buf

        (first_guess, next_guess) = unbundle $ mux try_guess (splitCell <$> cell) (bundle (cell, cell))
        guess_this = enable_propagate .&&. try_guess .&&. (not <$> is_unique)
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. (not <$> guess_this)

    fresh = {- register False $ -} isJust <$> shift_in .||. isJust <$> pop

    result =
        mux (not <$> new_round) (pure Progress) $
        mux fresh (pure Progress) $
        mux any_failed (pure Failure) $
        mux all_unique (pure Solved) $
        mux any_changed (pure Progress) $
        mux can_guess (pure Guess) $
        -- mux should_guess (pure Failure) $
        pure Progress
        -- pure Failure
