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

unzipMatrix :: Matrix n m (a, b) -> (Matrix n m a, Matrix n m b)
unzipMatrix = (FromRows *** FromRows) . unzip . fmap unzip . matrixRows

unzipGrid :: Grid n m (a, b) -> (Grid n m a, Grid n m b)
unzipGrid = (Grid *** Grid) . unzipMatrix . fmap unzipMatrix . getGrid

data PropagatorResult
    = Solved
    | Failure
    | Guess
    | Progress
    deriving (Generic, NFDataX, Eq, Show)

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
    , row_neighbour, col_neighbour, box_neighbour :: Mask n m
    , keep_guessing :: Bool
    } |]

type Solvable n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= n * m * m * n)

diag :: (KnownNat n) => Vec n (Vec n a) -> Vec n a
-- diag = zipWith (flip (!!)) indicesI
diag Nil = Nil
diag (Cons (Cons x xs) xss) = Cons x (diag (tail <$> xss))

foo
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
foo enable_propagate commit_guess shift_in pop = (head_cell, result, cells, can_guess, conts)
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
    changeds = changed <$> units
    conts = unflattenGrid $ cont <$> units

    faileds = fmap (.== conflicted) cells

    should_guess = not <$> any_changed
    can_guess = should_guess .&&. (not <$> keep_guessing)

    fresh = register False $ isJust <$> shift_in .||. isJust <$> pop
    all_unique = foldGrid (.&&.) uniques
    any_changed = fold @(n * m * m * n - 1) (.||.) changeds
    any_failed  = foldGrid (.||.) faileds

    result =
        mux fresh (pure Progress) $
        mux all_unique (pure Solved) $
        mux any_failed (pure Failure) $
        mux can_guess (pure Guess) $
        mux should_guess (pure Failure) $
        pure Progress

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
        cell' = load .<|>. enable (commit_guess .&&. guess_this) first_guess .<|>. enable enable_propagate (propagate cell neighbour_masks) .<|. cell
        changed = register False $ cell ./=. cell'
        is_unique = isUnique <$> cell

        (first_guess, next_guess) = unbundle $ mux try_guess (splitCell <$> cell) (bundle (cell, cell))

        can_guess = not <$> is_unique
        guess_this = enable_propagate .&&. try_guess .&&. can_guess
        cont = mux guess_this next_guess cell
        keep_guessing = try_guess .&&. (not <$> guess_this)
