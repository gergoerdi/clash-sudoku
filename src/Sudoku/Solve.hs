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
    :: forall n k. (KnownNat n, 1 <= n, KnownNat k)
    => forall k0. (k ~ k0 + 1)
    => Vec n (Square k)
    -> WriterT (Any, All) Maybe (Vec n (Square k))
simplify xs = traverse (uncurry simplifySquare) (zip xs (others xs))
  where
    simplifySquare :: forall n. Square k -> Vec n (Square k) -> WriterT (Any, All) Maybe (Square k)
    simplifySquare x xs = do
        guard $ x' /= 0
        tell (Any $ x' /= x, All $ isUnique x')
        pure x'
      where
        x' = foldl f x xs
        f x y | Unique{} <- getUnique y = x .&. complement y
              | otherwise = x

propagate1
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> WriterT (Any, All) Maybe (Sudoku n m)
propagate1 s = do
    (s :: Sudoku n m) <- rowwise  simplify s
    (s :: Sudoku n m) <- columnwise simplify s
    (s :: Sudoku n m) <- squarewise simplify s
    return s

commit1
    :: forall n m. (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => forall k. (KnownNat k, (n * m) ~ (k + 1))
    => Sudoku n m
    -> (Sudoku n m, Maybe (Sudoku n m))
commit1 s = (Sudoku next, Sudoku <$> after)
  where
    next = map (map fst) r
    after = traverse (traverse snd) r

    r = flip evalState False . traverse (traverse f) . getSudoku $ s

    f :: Square (n * m) -> State Bool (Square (n * m), Maybe (Square (n * m)))
    f x
      | Unique{} <- getUnique x = pure (x, Just x)
      | otherwise = do
            changed <- get
            let mb_i = elemIndex True (reverse . bitCoerce $ x)
            case (changed, mb_i) of
                (False, Just i) -> do
                    put True
                    let x' = oneHot i
                        x'' = x .&. complement x'
                    pure (x', x'' <$ guard (x'' /= 0))
                _ -> pure (x, Just x)

data Result n m
    = Working
    | Solution (Sudoku n m)
    | Unsolvable
    deriving (Generic, NFDataX)
deriving instance (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ (k + 1), k <= 8) => Show (Result n m)

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
