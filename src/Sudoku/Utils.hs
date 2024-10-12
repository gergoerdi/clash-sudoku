{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving, DerivingStrategies #-}
module Sudoku.Utils where

import Clash.Prelude
import Data.Monoid (Any(..), All(..))
import Data.Function (on)
import Data.Composition

enable :: (Applicative f) => f Bool -> f a -> f (Maybe a)
enable en x = mux en (Just <$> x) (pure Nothing)

packWrite :: addr -> Maybe val -> Maybe (addr, val)
packWrite addr val = (addr,) <$> val

reduceAll :: (BitPack a) => a -> All
reduceAll = All . bitToBool . reduceAnd

reduceAny :: (BitPack a) => a -> Any
reduceAny = Any . bitToBool . reduceOr

deriving anyclass instance BitPack All
deriving anyclass instance BitPack Any

instance SaturatingNum Bit where
    satAdd mode = unpack .: satAdd mode `on` pack
    satSub mode = unpack .: satSub mode `on` pack
    satMul mode = unpack .: satMul mode `on` pack
