{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Sudoku.Solve
    ( Result(..), Phase(..)
    , solver1
    , propagate
    , commit1
    ) where

import Clash.Prelude hiding (lift)

import Sudoku.Grid
import Sudoku.Serial
import Sudoku.Stack

import Control.Monad.Writer
import Control.Monad.State

others :: (1 <= n) => Vec n a -> Vec n (Vec (n - 1) a)
others (Cons x Nil) = Nil :> Nil
others (Cons x xs@(Cons _ _)) = xs :> map (x :>) (others xs)

propagateNeighbours
    :: forall n m k. (KnownNat m, KnownNat n, KnownNat k, 1 <= k)
    => Vec k (Cell n m)
    -> WriterT (Any, All) Maybe (Vec k (Cell n m))
propagateNeighbours xs = do
    tell (mempty, All solved)
    zipWithM propagateCell xs (foldl (.&.) maxBound <$> others xs')
  where
    zipWithM f xs ys = mapM (uncurry f) (zip xs ys)

    (xs', solveds) = unzip $ getSolved <$> xs
    solved = and solveds

    getSolved x = let b = isUnique x in (if b then cellBits x else 0, b)

    propagateCell x neighbours = do
        let x' = combine x neighbours
            changed = x' /= x
        guard $ x' /= conflicted
        tell (Any changed, mempty)
        pure x'

propagate
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= m * n)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> WriterT (Any, All) Maybe (Sudoku n m)
propagate = rowwise propagateNeighbours >=> columnwise propagateNeighbours >=> boxwise propagateNeighbours

commit1
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> (Sudoku n m, Maybe (Sudoku n m))
commit1 s = (next, after)
  where
    next = fmap fst r
    after = traverse snd r

    r = flip evalState False . traverse f $ s

    f :: Cell n m -> State Bool (Cell n m, Maybe (Cell n m))
    f x
      | isUnique x = pure (x, Just x)
      | otherwise = do
            changed <- get
            case (changed, splitCell x) of
                (False, Just (next, after)) -> do
                    put True
                    pure (next, after <$ guard (after /= conflicted))
                _ -> do
                    pure (x, Just x)

data Result n m
    = Working
    | Solution (Sudoku n m)
    | Unsolvable
    deriving (Generic, NFDataX)
-- deriving instance (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8) => Show (Result n m)

data Phase n m
    = Init
    | Propagate (Sudoku n m)
    | Try (Sudoku n m)
    | Solved (Sudoku n m)
    deriving (Generic, NFDataX)

solver1
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Maybe (Sudoku n m)
    -> State (Phase n m) (Maybe (Sudoku n m), Maybe (StackCmd (Sudoku n m)))
solver1 (Just popGrid) = do
    put $ Propagate popGrid
    return (Nothing, Nothing)
solver1 Nothing = get >>= \case
    Init -> do
        return (Nothing, Just Pop)
    Solved grid -> do
        return (Just grid, Nothing)
    Propagate grid -> case runWriterT $ propagate grid of
        Nothing -> do
            return (Nothing, Just Pop)
        Just (grid', (Any changed, All solved)) -> do
            put $ if
                | solved    -> Solved grid'
                | changed   -> Propagate grid'
                | otherwise -> Try grid'
            return (Nothing, Nothing)
    Try grid -> do
        let (next, after) = commit1 grid
        put $ Propagate next
        return (Nothing, Push <$> after)
