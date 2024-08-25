{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, RecordWildCards, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-} -- For declaring Barbie types and lenses
{-# LANGUAGE DerivingVia #-} -- For generic Monoid
module Sudoku.Solve
    ( Solvable
    , propagator, PropagatorCmd(..), PropagatorResult(..)
    , FooCmd(..), FooResult(..)
    ) where

import Clash.Prelude hiding (mapAccumR)
import RetroClash.Utils hiding (changed)
import RetroClash.Barbies

import Sudoku.Grid
import Sudoku.Matrix
import Sudoku.Stack

import Data.Maybe
import Barbies.TH
import Data.Monoid.Action
import Control.Monad.State -- .Strict
import Control.Monad.Writer.Strict
import Control.Lens hiding (Index)
import Data.Monoid (Last(..), All(..), Any(..))
import Data.Monoid.Generic
import Data.Foldable (traverse_, for_)

import Format (countSuccChecked) -- TODO

-- shiftInGridAtN :: forall n m a. (KnownNat n, KnownNat m) => Grid n m a -> a -> (a, Grid n m a)
-- shiftInGridAtN grid x = (x', unflattenGrid grid')
--   where
--     (grid', x' :> Nil) = shiftInAtN (flattenGrid grid) (x :> Nil)

-- neighboursMasks
--     :: forall n m dom. (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 2 <= n * m)
--     => (HiddenClockResetEnable dom)
--     => Grid n m (Signal dom (Mask n m))
--     -> (Signal dom Bool, Grid n m (Signal dom (Mask n m)))
-- neighboursMasks masks = (failed, combine <$> rows <*> columns <*> boxes)
--   where
--     (.<>.) = liftA2 (<>)

--     row_masks :: Vec (n * m) (Signal dom (Mask n m))
--     (row_failed, row_masks) = unzip $ rowmap combineRegion masks
--     (col_failed, col_masks) = unzip $ colmap combineRegion masks
--     (box_failed, box_masks) = unzip $ toRowMajorOrder $ boxmap combineRegion masks

--     rows = gridFromRows . fmap repeat $ row_masks
--     columns = gridFromRows . repeat $ col_masks
--     boxes = fromBoxes . fmap repeat $ fromRowMajorOrder box_masks

--     combine m1 m2 m3 = m1 .<>. m2 .<>. m3

--     failed = (any_row_failed .||. any_col_failed .||. any_box_failed)

--     any_row_failed = fold @(n * m - 1) (.||.) row_failed
--     any_col_failed = fold @(n * m - 1) (.||.) col_failed
--     any_box_failed = fold @(n * m - 1) (.||.) box_failed

-- combineRegion
--     :: forall n m dom. (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 1 <= n * m)
--     => Vec (n * m) (Signal dom (Mask n m)) -> (Signal dom Bool, Signal dom (Mask n m))
-- combineRegion = foldr f (pure False, pure mempty)
--   where
--     f :: Signal dom (Mask n m) -> (Signal dom Bool, Signal dom (Mask n m)) -> (Signal dom Bool, Signal dom (Mask n m))
--     f mask (failed, mask_acc) = (failed .||. overlapping <$> mask <*> mask_acc, mask .<>. mask_acc)

--     (.<>.) = liftA2 (<>)

combineRegion
    :: forall n m. (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 1 <= n * m)
    => Vec (n * m) (Mask n m) -> (Bool, Mask n m)
combineRegion = foldr f (False, mempty)
  where
    f :: Mask n m -> (Bool, Mask n m) -> (Bool, Mask n m)
    f mask (failed, mask_acc) = (failed || overlapping mask mask_acc, mask <> mask_acc)

declareBareB [d|
  data CellUnit n m = CellUnit
    { cell :: Cell n m
    , mask :: Mask n m
    , is_unique :: Bool
    -- , changed :: Bool
    -- , cont :: Cell n m
    -- , keep_guessing :: Bool
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

type Addr n m = Index (n * m * m * n)
type AtlasPtr n m = (Index (3 * n * m), Index (n * m))

data FooCmd (n :: Nat) (m :: Nat)
    = Input (Addr n m) (Cell n m)
    | Solve
    | Ask (Addr n m)

data FooResult (n :: Nat) (m :: Nat)
    = Idle
    | Busy
    | Failed
    | Solved_
    | Answer (Cell n m)

type GridSize n m = n * m * m * n
type StackSize n m = GridSize n m
type GridOp n m = RamOp (GridSize n m) (Cell n m)
type StackOp n m = RamOp (StackSize n m * GridSize n m) (Cell n m)

data FooOut n m = FooOut
    { _gridOp :: Last (GridOp n m)
    , _stackOp :: Last (StackOp n m)
    }
    deriving Generic
    deriving Semigroup via GenericSemigroup (FooOut n m)
    deriving Monoid via GenericMonoid (FooOut n m)
makeLenses ''FooOut

unpackOut :: (Monad t) => WriterT (FooOut n m) t a -> t (a, GridOp n m, StackOp n m)
unpackOut act = do
    (x, out@FooOut{..}) <- runWriterT act
    pure (x, fromMaybe RamNoOp $ getLast _gridOp, fromMaybe RamNoOp $ getLast _stackOp)

data FooPhase (n :: Nat) (m :: Nat)
    = Idling
    | Loading All Any (Index (3 * n * m)) (Maybe (Index (n * m)))
    | Propagating All Any (Index (3 * n * m))
    | WritingBack All Any (Index (3 * n * m)) (Maybe (Index (n * m)))
    | Guessing Bool (Index (GridSize n m))
    | Committing Bool (Index (GridSize n m))
    | Answering
    deriving (Generic, NFDataX)

data FooState n m = FooState
    { _phase :: FooPhase n m
    , _buffer :: Vec (n  * m) (Cell n m)
    }
    deriving (Generic, NFDataX)
makeLenses ''FooState

initState :: (Solvable n m) => FooState n m
initState = FooState
    { _phase = Idling
    , _buffer = repeat conflicted
    }

foo
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom (Maybe (FooCmd n m))
    -> Signal dom (FooResult n m)
foo cmd = result
  where
    (result, grid_op, stack_op) = mealySB (unpackOut . solve) (initState @n @m) (cmd, grid_rd, stack_rd)

    grid_rd :: Signal dom (Cell n m)
    grid_rd = blockRamU NoClearOnReset (SNat @(n * m * m * n)) undefined grid_addr grid_wr

    grid_addr :: Signal dom (Addr n m)
    grid_addr = do
        op <- grid_op
        pure $ case op of
            RamRead i -> i
            _ -> 0

    grid_wr :: Signal dom (Maybe (Addr n m, Cell n m))
    grid_wr = do
        op <- grid_op
        pure $ case op of
            RamWrite i x -> Just (i, x)
            _ -> Nothing

    stack_rd :: Signal dom (Cell n m)
    stack_rd = pure conflicted -- TODO

    stack_addr = do
        op <- stack_op
        pure $ case op of
            RamRead i -> i
            _ -> 0

    atlas = gridToRows row_atlas ++ gridToRows col_atlas ++ gridToRows box_atlas
      where
        row_atlas = gridFromRows @n @m (unconcatI indicesI)
        col_atlas = transposeGrid row_atlas
        box_atlas = fromBoxes @n @m . fromRowMajorOrder  . unconcatI $ indicesI

    goto phase' = phase .= phase'

    solve (cmd, grid_rd, stack_rd) = use phase >>= \case
        Idling -> case cmd of
            Just (Input i x) -> do
                scribe gridOp $ pure $ RamWrite i x
                pure Busy
            Just Solve -> do
                goto $ Loading mempty mempty 0 Nothing
                pure Busy
            Just (Ask i) -> do
                goto Answering
                scribe gridOp $ pure $ RamRead i
                pure Busy
            Nothing -> do
                pure Idle
        Answering -> do
            goto Idling
            pure $ Answer grid_rd
        Loading all_solved any_changed i j -> do
            j' <- case j of
                Nothing -> pure $ Just 0
                Just j -> do
                    buffer %= replace j grid_rd
                    pure $ countSuccChecked j
            goto $ maybe (Propagating all_solved any_changed i) (Loading all_solved any_changed i . Just) j'
            scribe gridOp $ pure $ maybe RamNoOp (RamRead . (atlas !! i !!)) j'
            pure Busy
        Propagating all_solved any_changed i -> do
            (solved, failed, changed, buffer') <- propagate <$> use buffer
            buffer .= buffer'
            if | getAny failed -> do
                     -- TODO: backtrack
                     pure Busy
               | getAny changed -> do
                     goto $ Propagating all_solved (any_changed <> changed) i
                     pure Busy
               | otherwise -> do
                     goto $ WritingBack (all_solved <> solved) any_changed i Nothing
                     pure Busy
        WritingBack all_solved any_changed i j -> do
            case maybe (Just 0) countSuccChecked j of
                Just j' -> do
                    cell <- (!! j') <$> use buffer
                    scribe gridOp $ pure $ RamWrite (atlas !! i !! j') cell
                    goto $ WritingBack all_solved any_changed i (Just j')
                    pure Busy
                Nothing -> do
                    case countSuccChecked i of
                        Just i' -> do
                            goto $ Loading all_solved any_changed i' Nothing
                            pure Busy
                        Nothing | getAll all_solved -> do
                            goto Idling
                            pure Solved_
                        Nothing -> do
                            -- TODO: guess if nothing has changed
                            --         -- goto $ Guessing False 0
                            --         -- scribe gridOp $ pure $ RamRead 0

                            -- Start new propagation
                            goto $ Loading mempty mempty 0 Nothing
                            pure Busy
            pure Busy
        Guessing guessed i -> do
            let (guess, cont) = splitCell grid_rd
                can_guess = not guessed && cont /= conflicted
                cont' = if can_guess then cont else grid_rd
            when can_guess do
                scribe gridOp $ pure $ RamWrite i guess
            goto $ Committing (guessed || can_guess) i
            -- scribe stackOp $ pure $ RamWrite i cont' -- TODO: push continuation
            pure Busy
        Committing guessed i -> do
            let i' = countSuccChecked i
            traverse_ (scribe gridOp . pure . RamRead) i'
            goto $ maybe (Loading mempty mempty 0 Nothing) (Guessing guessed) i'
            pure Busy

    propagate :: Vec (n * m) (Cell n m) -> (All, Any, Any, Vec (n * m) (Cell n m))
    propagate cells = (All . bitToBool . reduceAnd $ uniques, Any failed, Any . bitToBool . reduceOr $ changeds, cells')
      where
        isUnique cell = snd (splitCell cell) == conflicted

        uniques = isUnique <$> cells

        maskOf is_unique cell
            | is_unique = cellMask cell
            | otherwise = mempty

        masks = maskOf <$> uniques <*> cells

        (failed, mask) = combineRegion masks

        applyMask is_unique cell
            | is_unique = (False, cell)
            | otherwise = (changed, cell')
          where
            cell' = act mask cell
            changed = cell' /= cell

        (changeds, cells') = unzip $ applyMask <$> uniques <*> cells

propagator
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom (Maybe PropagatorCmd)
    -> Signal dom (Maybe (Addr n m))
    -> Signal dom (Maybe (Cell n m))
    -> Signal dom (Maybe (Sudoku n m))
    -> ( Signal dom (Cell n m)
       , Signal dom PropagatorResult
       , Grid n m (Signal dom (Cell n m))
       )
propagator cmd load_addr load_value pop = undefined
-- propagator cmd load_addr load_value pop = (grid_rd, undefined, undefined)
--   where
--     grid_rd :: Signal dom (Cell n m)
--     grid_rd = blockRamU NoClearOnReset (SNat @(n * m * m * n)) undefined grid_addr grid_wr

--     grid_addr :: Signal dom (Addr n m)
--     grid_addr = undefined

--     grid_wr :: Signal dom (Maybe (Addr n m, Cell n m))
--     grid_wr = do
--         load_addr <- load_addr
--         load_value <- load_value

--         pure if
--             | Just addr <- load_addr, Just value <- load_value -> Just (addr, value)
--             -- | undefined -> write back from window
--             | otherwise -> Nothing

--     window :: Vec (n * m) (Signals dom (CellUnit n m))
--     window = map mkCell indicesI

--     (window_failed, window_mask) = combineRegion $ mask <$> window

--     mkCell :: Index (n * m) -> Signals dom (CellUnit n m)
--     mkCell i = CellUnit{..}
--       where
--         cell = register conflicted cell'
--         mask = mux is_unique (cellMask <$> cell) (pure mempty)
--         is_unique = (fst . splitCell <$> cell) .==. cell

--         cell' = do
--             cmd <- cmd
--             old <- cell
--             read <- grid_rd
--             propagate <- act <$> window_mask <*> cell
--             is_unique <- is_unique

--             pure if
--                 -- | undefined -> read into window
--                 | Just Propagate <- cmd , not is_unique -> propagate
--                 | otherwise -> old

atlas :: forall n m. (Solvable n m) => SNat n -> SNat m -> Vec (n * m * 3) (Vec (n * m) (Addr n m))
atlas n m = gridToRows row_atlas ++ gridToRows col_atlas ++ gridToRows box_atlas
  where
    row_atlas = gridFromRows @n @m (unconcatI indicesI)
    col_atlas = transposeGrid row_atlas
    box_atlas = fromBoxes @n @m . fromRowMajorOrder  . unconcatI $ indicesI


-- propagator cmd shift_in pop = (head @(n * m * m * n - 1) (flattenGrid cells), result, conts)
--   where
--     pops :: Grid n m (Signal dom (Maybe (Cell n m)))
--     pops = unbundle . fmap sequenceA $ pop

--     (any_failed, neighbours_masks) = neighboursMasks masks

--     units :: Grid n m (Signals dom (CellUnit n m))
--     units = pure unit <*> shift_ins <*> prev_guesses <*> pops <*> neighbours_masks

--     (_shift_out, shift_ins) = shiftInGridAtN (enable (isJust <$> shift_in) . cell <$> units) shift_in
--     (_guessing_failed, prev_guesses) = shiftInGridAtN (keep_guessing <$> units) (pure True)

--     masks = mask <$> units
--     cells = cell <$> units
--     conts = cont <$> units

--     fresh = isJust <$> shift_in .||. isJust <$> pop
--     all_unique = bitToBool . reduceAnd <$> bundle (is_unique <$> units)
--     any_changed = bitToBool . reduceOr <$> bundle (changed <$> units)
--     can_guess = not <$> all_unique

--     result =
--         mux fresh (pure Progress) $
--         mux any_failed (pure Failure) $
--         mux all_unique (pure Solved) $
--         mux any_changed (pure Progress) $
--         mux can_guess (pure Guess) $
--         pure Failure

--     (enable_propagate, enable_guess) = unbundle do
--         cmd <- cmd
--         pure $ case cmd of
--             Just Propagate -> (True, False)
--             Just CommitGuess -> (False, True)
--             _ -> (False, False)

--     unit
--         :: Signal dom (Maybe (Cell n m))
--         -> Signal dom Bool
--         -> Signal dom (Maybe (Cell n m))
--         -> Signal dom (Mask n m)
--         -> Signals dom (CellUnit n m)
--     unit shift_in try_guess pop neighbours_mask = CellUnit{..}
--       where
--         shift_out = enable (isJust <$> shift_in) cell

--         cell = register conflicted cell'
--         mask = mux is_unique (cellMask <$> cell) (pure mempty)
--         propagated = act <$> neighbours_mask <*> cell

--         cell' = do
--             shift_in <- shift_in
--             pop <- pop
--             can_propagate <- enable_propagate .&&. not <$> is_unique
--             use_guess <- enable_guess .&&. guess_this
--             guess <- first_guess
--             propagated <- propagated
--             old <- cell
--             pure if
--                 | Just load <- shift_in -> load
--                 | Just load <- pop      -> load
--                 | use_guess             -> guess
--                 | can_propagate         -> propagated
--                 | otherwise             -> old

--         changed = cell' ./=. cell

--         (first_guess, next_guess) = unbundle $ splitCell <$> cell
--         is_unique = next_guess .== conflicted

--         can_guess = not <$> is_unique
--         guess_this = try_guess .&&. can_guess
--         cont = mux guess_this next_guess cell
--         keep_guessing = try_guess .&&. is_unique
