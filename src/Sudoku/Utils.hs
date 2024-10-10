{-# LANGUAGE TupleSections, LambdaCase #-}
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

instance SaturatingNum Bit where
    satAdd = \case
        SatWrap -> addOr 0
        SatBound -> addOr maxBound
        SatZero ->  addOr 0
        SatSymmetric -> addOr maxBound
        SatError -> addOr $ errorX "satAdd"
      where
        addOr ext = \x y -> if x == 1 && y == 1 then ext else x + y

    satSub = \case
        SatWrap -> subOr 1
        SatBound -> subOr minBound
        SatZero ->  subOr 0
        SatSymmetric -> subOr maxBound
        SatError -> subOr $ errorX "satSub"
      where
        subOr ext = \x y -> if x == 0 && y == 1 then ext else x - y

    satMul = \case
        _ -> (*)
