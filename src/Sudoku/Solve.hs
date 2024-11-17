{-# LANGUAGE BlockArguments, ViewPatterns, MultiWayIf, LambdaCase #-}
module Sudoku.Solve
    ( Sudoku
    , Solvable

    , consistent

    , solver
    , Result(..)
    ) where

import Clash.Prelude hiding (mapAccumR)

import Sudoku.Utils
import Sudoku.Grid
import Sudoku.Cell

import Data.Monoid.Action
import Data.Traversable (mapAccumR)

type Sudoku n m = Grid n m (Cell n m)
type Solvable n m = (KnownNat n, KnownNat m, 1 <= n * m * m * n)

consistent :: (KnownNat n, KnownNat m, KnownNat k) => Vec k (Mask n m) -> Bool
consistent = not . bitsOverlap . fmap maskBits

expand :: (KnownNat n, KnownNat m) => Sudoku n m -> (Grid n m Bool, Sudoku n m, Sudoku n m)
expand = funzip3 . snd . mapAccumR guess False
  where
    guess guessed_before cell
        | not guessed_before && not single
        = (True, (single, first_guess, next_guess))

        | otherwise
        = (guessed_before, (single, cell, cell))
      where
        (first_guess, next_guess) = splitCell cell
        single = next_guess == conflicted

data Result n m
    = Blocked
    | Complete
    | Progress (Sudoku n m)
    | Stuck (Sudoku n m)
    deriving (Generic, NFDataX)

solve :: (KnownNat n, KnownNat m) => Sudoku n m -> (Result n m, Sudoku n m)
solve grid = (result, next_guess)
  where
    result
        | blocked   = Blocked
        | complete  = Complete
        | changed   = Progress pruned
        | otherwise = Stuck first_guess

    blocked = void || not safe
    void = any (== conflicted) grid
    safe = allGroups consistent masks
    complete = and singles

    (singles, first_guess, next_guess) = expand grid
    pruned = apply <$> group_masks <*> singles <*> grid

    masks = maskOf <$> singles <*> grid
    group_masks = foldGroups masks
    changed = pruned /= grid

    maskOf single cell = if single then cellMask cell else mempty
    apply mask single cell = if single then cell else act mask cell

shiftIn :: (KnownNat n, KnownNat m) => Maybe a -> Grid n m a -> Maybe (Grid n m a)
shiftIn shift_in = sequenceA . snd . mapAccumR shift shift_in
  where
    shift shift_in cell = case shift_in of
        Nothing -> (Nothing, Nothing)
        Just shift_in -> (Just cell, Just shift_in)

commit :: Maybe (Sudoku n m) -> Maybe (Sudoku n m) -> Bool -> Result n m -> Maybe (Sudoku n m)
commit shifted popped en result = shifted <|> popped <|> solved
  where
    solved
        | Progress pruned <- result, en   = Just pruned
        | Stuck first_guess <- result, en = Just first_guess
        | otherwise                       = Nothing

solver
    :: forall n m dom. (Solvable n m, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (Sudoku n m))
    -> Signal dom (Maybe (Cell n m))
    -> ( Signal dom (Result n m)
       , Signal dom (Cell n m)
       , Signal dom (Sudoku n m)
       )
solver en popped shift_in = (result, headGrid cells, next_guess)
  where
    cells = regMaybe wild <$> cells'

    grid = bundle cells
    shifted = shiftIn <$> shift_in <*> grid
    (result, next_guess) = unbundle $ solve <$> grid
    cells' = unbundle . fmap sequenceA $ commit <$> shifted <*> popped <*> en <*> result
