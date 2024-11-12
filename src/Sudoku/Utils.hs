{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies #-}
module Sudoku.Utils where

import Clash.Prelude
import Clash.Class.Counter.Internal
import Data.Function (on)
import Data.Composition
import Control.Monad (guard)

countPredChecked :: Counter a => a -> Maybe a
countPredChecked x = x' <$ guard (not underflow)
  where
    (underflow, x') = countPredOverflow x

enable :: (Applicative f, Alternative g) => f Bool -> f a -> f (g a)
enable en x = mux en (pure <$> x) (pure empty)

infixl 4 .<$>., .<*>.
(.<$>.) :: (Applicative f, Applicative g) => (a -> b) -> f (g a) -> f (g b)
(.<$>.) f = liftA ((<$>) f)

(.<*>.) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(.<*>.) = liftA2 (<*>)

funzip3 :: (Functor f) => f (a, b, c) -> (f a, f b, f c)
funzip3 xyzs = ((\(x, y, z) -> x) <$> xyzs, (\(x, y, z) -> y) <$> xyzs, (\(x, y, z) -> z) <$> xyzs)

packWrite :: addr -> Maybe val -> Maybe (addr, val)
packWrite addr val = (addr,) <$> val

instance SaturatingNum Bit where
    satAdd mode = unpack .: satAdd mode `on` pack
    satSub mode = unpack .: satSub mode `on` pack
    satMul mode = unpack .: satMul mode `on` pack
