{-# LANGUAGE UndecidableInstances #-}
module Sudoku.Hacks
  ( mapAccumR
  ) where

import Clash.Prelude hiding (mapAccumR)
import Clash.Class.Counter.Internal

-- Workaround for https://github.com/clash-lang/clash-compiler/issues/1111 :
-- a `mapAccumR` without a corresponding black box
mapAccumR :: (acc -> a -> (acc, b)) -> acc -> Vec n a -> (acc, Vec n b)
mapAccumR _ s Nil = (s, Nil)
mapAccumR f s (Cons x xs) =
    let (s', ys) = mapAccumR f s xs
        (s'', y) = f s' x
    in (s'', Cons y ys)

-- Backport of https://github.com/clash-lang/clash-compiler/pull/278
rippleR :: (a -> (Bool, a)) -> Vec n a -> (Bool, Vec n a)
rippleR f = mapAccumR step True
  where
    step carry x = if carry then f x else (False, x)

instance (Counter a, KnownNat n, 1 <= n) => Counter (Vec n a) where
    countMin = repeat countMin
    countMax = repeat countMax

    countSuccOverflow = rippleR countSuccOverflow
    countPredOverflow = rippleR countPredOverflow
