module Sudoku.Iso
    ( module Sudoku.Iso
    , module Data.Isomorphism
    , module Data.Groupoid
    ) where

import Clash.Prelude
import Data.Coerce
import Data.Isomorphism
import Data.Groupoid

infixr 0 <->
type (<->) = Iso (->)

icoerce :: (Coercible a b) => a <-> b
icoerce = Iso coerce coerce

imap :: (Functor f) => a <-> b -> f a <-> f b
imap phi = Iso (fmap $ embed phi) (fmap $ project phi)
