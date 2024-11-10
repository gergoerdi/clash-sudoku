{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies #-}
module Sudoku.Utils where

import Clash.Prelude
import Clash.Class.Counter.Internal
import Data.Function (on)
import Data.Composition
import Control.Monad (guard)
import Control.Monad.State.Strict

countPredChecked :: Counter a => a -> Maybe a
countPredChecked x = x' <$ guard (not underflow)
  where
    (underflow, x') = countPredOverflow x

enable :: (Applicative f, Alternative g) => f Bool -> f a -> f (g a)
enable en x = mux en (pure <$> x) (pure empty)

traverseS
    :: (Traversable f, HiddenClockResetEnable dom)
    => (a -> s -> (b, s))
    -> Signal dom s
    -> f (Signal dom a)
    -> f (Signal dom b)
traverseS step s0 grid = evalState (traverse (state . fmap unbundle . liftA2 step) grid) s0

packWrite :: addr -> Maybe val -> Maybe (addr, val)
packWrite addr val = (addr,) <$> val

instance SaturatingNum Bit where
    satAdd mode = unpack .: satAdd mode `on` pack
    satSub mode = unpack .: satSub mode `on` pack
    satMul mode = unpack .: satMul mode `on` pack
