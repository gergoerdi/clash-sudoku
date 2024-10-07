{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving, DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Sudoku.Utils where

import Clash.Prelude
import Data.Monoid (Any(..), All(..))

enable :: (Applicative f) => f Bool -> f a -> f (Maybe a)
enable en x = mux en (Just <$> x) (pure Nothing)

packWrite :: addr -> Maybe val -> Maybe (addr, val)
packWrite addr val = (addr,) <$> val

reduceAll :: (BitPack a) => a -> All
reduceAll = All . bitToBool . reduceAnd

reduceAny :: (BitPack a) => a -> Any
reduceAny = Any . bitToBool . reduceOr

deriving via Bool instance BitPack All
deriving via Bool instance BitPack Any
