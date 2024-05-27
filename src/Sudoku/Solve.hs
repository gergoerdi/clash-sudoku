{-# LANGUAGE BlockArguments, LambdaCase, MultiWayIf #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Sudoku.Solve
    ( Result(..), Phase(..)
    , solver1
    , propagate1
    , commit1
    ) where

import Clash.Prelude hiding (lift)

import Sudoku.Board
import Sudoku.Serial
import Sudoku.Stack

import Control.Monad.Writer
import Control.Monad.State

others :: (1 <= n) => Vec n a -> Vec n (Vec (n - 1) a)
others (Cons x Nil) = Nil :> Nil
others (Cons x xs@(Cons _ _)) = xs :> map (x :>) (others xs)

simplify
    :: forall n m k. (KnownNat m, KnownNat n, KnownNat k, 1 <= k)
    => Vec k (Space n m)
    -> WriterT (Any, All) Maybe (Vec k (Space n m))
simplify xs = zipWithM simplifySpace xs (others xs)
  where
    simplifySpace :: forall k. Space n m -> Vec k (Space n m) -> WriterT (Any, All) Maybe (Space n m)
    simplifySpace x xs = do
        guard $ x' /= conflicted
        tell (Any changed, All solved)
        pure x'
      where
        x' = combine x $ fmap (\x -> if isUnique x then x else conflicted) xs
        changed = x' /= x
        solved = isUnique x'

propagate1
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> WriterT (Any, All) Maybe (Sudoku n m)
propagate1 = rowwise simplify >=> columnwise simplify >=> fieldwise simplify

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

    f :: Space n m -> State Bool (Space n m, Maybe (Space n m))
    f x
      | isUnique x = pure (x, Just x)
      | otherwise = do
            changed <- get
            case (changed, splitSpace x) of
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
solver1 (Just popBoard) = do
    put $ Propagate popBoard
    return (Nothing, Nothing)
solver1 Nothing = get >>= \case
    Init -> do
        return (Nothing, Just Pop)
    Solved board -> do
        return (Just board, Nothing)
    Propagate board -> case runWriterT $ propagate1 board of
        Nothing -> do
            return (Nothing, Just Pop)
        Just (board', (Any changed, All solved)) -> do
            put $ if
                | solved    -> Solved board'
                | changed   -> Propagate board'
                | otherwise -> Try board'
            return (Nothing, Nothing)
    Try board -> do
        let (next, after) = commit1 board
        put $ Propagate next
        return (Nothing, Push <$> after)
