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

enable :: (Applicative f) => f Bool -> f a -> f (Maybe a)
enable en x = mux en (Just <$> x) (pure Nothing)

(.<|>.) :: (Applicative f, Alternative g) => f (g a) -> f (g a) -> f (g a)
(.<|>.) = liftA2 (<|>)

packWrite :: addr -> Maybe val -> Maybe (addr, val)
packWrite addr val = (addr,) <$> val

instance SaturatingNum Bit where
    satAdd mode = unpack .: satAdd mode `on` pack
    satSub mode = unpack .: satSub mode `on` pack
    satMul mode = unpack .: satMul mode `on` pack
