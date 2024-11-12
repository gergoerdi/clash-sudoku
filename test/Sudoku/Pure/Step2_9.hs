-- Compute `single` via `splitCell`
{-# LANGUAGE RecordWildCards, LambdaCase, BlockArguments #-}
module Sudoku.Pure.Step2_9 where

import Clash.Prelude hiding (lift)

import Sudoku.Solve (Solvable, Sudoku, bitsOverlap)
import Sudoku.Cell
import Sudoku.Grid

import Control.Monad
import Data.Monoid.Action
import Control.Monad.State.Strict
import Control.Arrow (first, second)
import Control.Arrow.Transformer.Automaton

import Debug.Trace

data Result n m
    = Blocked
    | Complete
    | Progress (Sudoku n m)
    | Guess (Sudoku n m) (Sudoku n m)

solve :: (Solvable n m) => Sudoku n m -> Result n m
solve grid
    | void || not safe  = Blocked
    | and singles       = Complete
    | pruned /= grid    = Progress pruned
    | otherwise         = Guess grid1 grid2
  where
    void = any (== conflicted) grid
    safe = allGroups consistent masks

    consistent = not . bitsOverlap . fmap maskBits

    splits = splitCell <$> grid
    singles = (== conflicted) . snd <$> splits

    masks = maskOf <$> singles <*> grid
    group_masks = foldGroups masks

    pruned = apply <$> singles <*> group_masks <*> grid

    maskOf single = if single then cellMask else mempty
    apply single mask = if single then id else act mask

    guesses = evalState (traverse (state . guess1) ((,,) <$> singles <*> grid <*> guesses)) False
    (grid1, grid2) = (fst <$> guesses, snd <$> guesses)

    guess1 (single, cell, guess) guessed_before 
        | not guessed_before
        , not single
        = (guess, True)

        | otherwise
        = ((cell, cell), guessed_before)

type Stack n m = [Sudoku n m]

data Cmd n m
    = Done -- (Sudoku n m)
    | Push (Sudoku n m)
    | Pop

-- machine :: (Solvable n m) => Sudoku n m -> Automaton (->) (Maybe (Sudoku n m)) (Maybe (Cmd n m))
-- machine = step
--   where
--     step grid = Automaton \case
--         Just load -> (Nothing, step load)
--         Nothing -> case solve grid of
--             Blocked -> (Just Pop, step grid)
--             Complete -> (Just $ Done grid, step grid)
--             Progress grid' -> (Nothing, step grid')
--             Guess grid1 grid2 -> (Just $ Push grid2, step grid1)

-- sudoku :: (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
-- sudoku grid = consume [] (machine grid) Nothing
--   where
--     consume stack step load = let Automaton f = step; (cmd, step') = f load in case cmd of
--         Nothing -> consume stack step' Nothing
--         Just (Done grid) -> pure grid
--         Just (Push grid) -> consume (grid:stack) step' Nothing
--         Just Pop -> case stack of
--             (grid:stack') -> consume stack' step' (Just grid)
--             [] -> empty



machine :: (Solvable n m) => Maybe (Sudoku n m) -> State (Sudoku n m) (Maybe (Cmd n m))
machine = \case
    Just load -> Nothing <$ put load
    Nothing -> solve <$> get >>= \case
        Blocked -> Just Pop <$ pure ()
        Complete -> Just Done <$ pure ()
        Progress grid' -> Nothing <$ put grid'
        Guess grid1 grid2 -> Just (Push grid2) <$ put grid1


-- sudoku :: forall n m. (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
-- sudoku grid = case runState (evalStateT (loop Nothing) []) grid of
--     (True, grid') -> pure grid'
--     (False, _) -> empty
--   where
--     loop :: Maybe (Sudoku n m) -> StateT (Stack n m) (State (Sudoku n m)) Bool
--     loop load = do
--         cmd <- lift $ machine load
--         case cmd of
--             Nothing -> do
--                 loop Nothing
--             Just Done -> do
--                 pure True
--             Just (Push grid) -> do
--                 modify (grid:)
--                 loop Nothing
--             Just Pop -> get >>= \case
--                 [] -> pure False
--                 (grid:stack) -> do
--                     put stack
--                     loop $ Just grid

controller :: (Monad f) => (Maybe (Sudoku n m) -> f (Maybe (Cmd n m))) -> f Bool
controller solver = evalStateT (loop Nothing) []
  where
    loop load = do
        cmd <- lift $ solver load
        case cmd of
            Nothing -> do
                loop Nothing
            Just Done -> do
                pure True
            Just (Push grid) -> do
                modify (grid:)
                loop Nothing
            Just Pop -> get >>= \case
                [] -> pure False
                (grid:stack) -> do
                    put stack
                    loop $ Just grid

sudoku :: forall n m. (Solvable n m) => Sudoku n m -> Maybe (Sudoku n m)
sudoku grid = case runState (controller machine) grid of
    (True, grid') -> pure grid'
    (False, _) -> empty
