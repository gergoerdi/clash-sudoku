module Sudoku.Iso
    ( module Sudoku.Iso
    , module Data.Isomorphism
    , module Data.Groupoid
    ) where

import Clash.Prelude
import Data.Coerce
import Data.Isomorphism
import Data.Groupoid

icoerce :: (Coercible a b) => Iso (->) a b
icoerce = Iso coerce coerce

imap :: (Functor f) => Iso (->) a b -> Iso (->) (f a) (f b)
imap iso = Iso (fmap $ embed iso) (fmap $ project iso)
