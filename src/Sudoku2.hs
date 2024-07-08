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
    | Busy
    | ShiftOut (Cnt n m)
    deriving (Generic, NFDataX, Show, Eq)

circuit
    :: forall n m dom k. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= n * m, n * m * m * n ~ k + 1, 1 <= StackSize n m)
    => (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Cell n m))
    -> Signal dom Bool
    -> ( ( Signal dom (Maybe (Cell n m))
        , Signal dom Bool
        )
      , ( Signal dom (Sudoku n m)
        , Signal dom Bool
        , Signal dom PropagatorResult
        )
      )
circuit shift_in out_ready = ((shift_out, in_ready), dbg)
  where
    dbg = (bundle grid, enable_propagate, result)

    shift_in' :: Signal dom (Maybe (Cell n m))
    (shift_in', in_ready, enable_propagate) = mealyStateB step (ShiftIn @n @m 0) (shift_in, out_ready, register Progress result)

    step (shift_in, out_ready, result) = do
        (traceShowId <$> get) >>= \case
            ShiftIn i -> do
                when (isJust shift_in) $ put $ maybe Busy ShiftIn $ countSuccChecked i
                pure (shift_in, True, False)
            Busy -> do
                case result of
                    Guess -> do
                        pure (Nothing, False, True)
                    Failure -> do
                        pure (Nothing, False, True)
                    Progress -> do
                        pure (Nothing, False, True)
                    Solved -> do
                        put $ ShiftOut 0
                        pure (Nothing, False, True)
            ShiftOut i -> do
                when out_ready $ put $ maybe (ShiftIn 0) ShiftOut $ countSuccChecked i
                pure (Just conflicted, False, False)

    (shift_out, result, grid, can_guess, next_guesses) = propagator enable_propagate shift_in' (pure Nothing)

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
    | Just v <- V.fromList ys
    = unflattenGrid v

    | otherwise
    = postproc xs
