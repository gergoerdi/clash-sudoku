{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf, ApplicativeDo #-}
module Sudoku2 where

import Clash.Prelude hiding (lift)
import Clash.Annotations.TH
import Clash.Class.Counter

import Data.Traversable (for)
import Control.Arrow (second, (***))
import Data.Maybe
import Control.Monad.State
import Control.Arrow.Transformer.Automaton
import qualified Data.List as L
import qualified Clash.Sized.Vector as V

import Sudoku.Matrix
import Sudoku.Grid
import Sudoku.Serial
import Sudoku.Solve hiding (Propagate)
import Sudoku.Stack

import RetroClash.Utils
import RetroClash.SerialRx
import RetroClash.SerialTx

import Debug.Trace

type StackSize n m = ((n * m) * (m * n))
type Cnt n m = Index ((n * m) * (m * n))

data St n m
    = ShiftIn (Cnt n m)
    | Busy (Index (StackSize n m))
    | WaitPush (Index (StackSize n m))
    | ShiftOut (Cnt n m)
    deriving (Generic, NFDataX, Show, Eq)

circuit
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, 1 <= m * m * m * n, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Cell n m))
    -> Signal dom Bool
    -> ( ( Signal dom (Maybe (Cell n m))
        , Signal dom Bool
        )
      , ( Signal dom (Sudoku n m)
        , Signal dom Bool
        , Signal dom Bool
        , Signal dom PropagatorResult
        , Signal dom (Maybe (StackCmd ()))
        , Signal dom (Maybe ())
        , Signal dom (Maybe (Cell n m))
        )
      )
circuit shift_in out_ready = ((shift_out', in_ready), dbg)
  where
    dbg = (bundle _grid, enable_propagate, commit_guess, result, fmap (() <$) <$> stack_cmd', (() <$) <$> stack_rd, shift_out')

    shift_in' :: Signal dom (Maybe (Cell n m))
    (shift_in', out_enabled, in_ready, enable_propagate, commit_guess, stack_cmd) = mealyStateB step (ShiftIn @n @m 0) (shift_in, out_ready, {- register Progress -} result, sp)
    shift_out' = guardA out_enabled shift_out

    -- step :: (Maybe (Cell n m), Bool, PropagatorResult, Index (StackSize n m)) -> State (St n m) (Maybe (Cell n m), Bool, Bool, Sudoku n m -> Maybe (StackCmd (Sudoku n m)))
    step (shift_in, out_ready, result, sp) = do
        get >>= \case
            ShiftIn i -> do
                when (isJust shift_in) $ put $ maybe (Busy sp) ShiftIn $ countSuccChecked i
                pure (shift_in, False, True, False, False, const Nothing)
            WaitPush top_sp -> do
                put $ Busy top_sp
                pure (Nothing, False, False, True, True, Just . Push)
            Busy top_sp -> do
                case result of
                    Guess -> do
                        put $ WaitPush top_sp
                        pure (Nothing, False, False, True, False, Just . Push)
                    Failure -> do
                        pure (Nothing, False, False, True, False, const $ Just Pop)
                    Progress -> do
                        pure (Nothing, False, False, True, False, const Nothing)
                    Solved -> do
                        put $ ShiftOut 0
                        pure (Nothing, False, False, True, False, const Nothing)
            ShiftOut i -> do
                when out_ready $ put $ maybe (ShiftIn 0) ShiftOut $ countSuccChecked i
                pure (Just conflicted, True, False, False, False, const Nothing)

    (shift_out, result, _grid, can_guess, next_guesses) = propagator (register False enable_propagate) (commit_guess) shift_in' popped
    popped = stack_rd

    -- stack_cmd' :: Signal dom (Maybe (StackCmd (Sudoku n m)))
    stack_cmd' = stack_cmd <*> bundle next_guesses

    -- sp :: Signal dom (Index (StackSize n m))
    (stack_rd, sp) = stack (SNat @(StackSize n m)) (emptySudoku @n @m) stack_cmd'


-- test :: Sudoku 3 3 -> [Sudoku 3 3]
test grid = load (toList $ flattenGrid grid) [] $ start (Nothing, True)
  where
    Automaton start = signalAutomaton (bundle . (bundle *** bundle) . uncurry (circuit @3 @3 @System) . unbundle)

    load xs ys (((shift_out, in_ready), dbg), Automaton step)
      | (x:xs') <- xs
      , in_ready
      = (dbg, ys) : load xs' ys (step (Just x, True))

      | Just y <- shift_out
      = (dbg, y:ys) : load xs (y:ys) (step (Nothing, True))

      | otherwise
      = (dbg, ys) : load xs ys (step (Nothing, True))



postproc ((dbg, ys):xs)
    | Just v <- V.fromList (L.reverse ys)
    = unflattenGrid v

    | otherwise
    = postproc xs
