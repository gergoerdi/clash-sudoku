{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies #-}
module Sudoku.Utils where

import Clash.Prelude hiding (mapAccumR)
import Clash.Class.Counter.Internal
import Data.Function (on)
import Data.Composition
import Control.Monad (guard)
import Control.Monad.State.Strict
import Data.Traversable

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

mapAccumRS
    :: (Traversable f, HiddenClockResetEnable dom)
    => (s -> a -> (s, b))
    -> Signal dom s
    -> f (Signal dom a)
    -> (Signal dom s, f (Signal dom b))
mapAccumRS f s = mapAccumR (fmap unbundle . liftA2 f) s

packWrite :: addr -> Maybe val -> Maybe (addr, val)
packWrite addr val = (addr,) <$> val

instance SaturatingNum Bit where
    satAdd mode = unpack .: satAdd mode `on` pack
    satSub mode = unpack .: satSub mode `on` pack
    satMul mode = unpack .: satMul mode `on` pack
