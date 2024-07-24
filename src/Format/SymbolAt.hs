{-# LANGUAGE UndecidableInstances #-}
module Format.SymbolAt where

import Clash.Prelude
import Data.Proxy

class SymbolLength_' (s :: Maybe (Char, Symbol)) where
    type SymbolLength' s :: Nat

    symbolAt :: proxy s -> Index (SymbolLength' s) -> Char

instance SymbolLength_' Nothing where
    type SymbolLength' Nothing = 0

    symbolAt _ i = errorX "impossible"

instance (SymbolLength_ s, KnownChar c, KnownNat (SymbolLength s)) => SymbolLength_' (Just '(c, s)) where
    type SymbolLength' (Just '(c, s)) = 1 + SymbolLength s

    symbolAt _ i
        | i == 0
        = charVal (Proxy @c)

        | otherwise
        = symbolAt (Proxy @(UnconsSymbol s)) (fromIntegral (i - 1))

type SymbolLength s = SymbolLength' (UnconsSymbol s)
type SymbolLength_ s = SymbolLength_' (UnconsSymbol s)
