{-# LANGUAGE UndecidableInstances #-}
module Format.SymbolAt (SymbolLength, IndexableSymbol, symbolAt) where

import Clash.Prelude
import Data.Proxy

class (KnownNat (SymbolLength' s)) => IndexableSymbol' (s :: Maybe (Char, Symbol)) where
    type SymbolLength' s :: Nat

    symbolAt :: proxy s -> Index (SymbolLength' s) -> Char

instance IndexableSymbol' Nothing where
    type SymbolLength' Nothing = 0

    symbolAt _ i = errorX "impossible"

instance (IndexableSymbol s, KnownChar c) => IndexableSymbol' (Just '(c, s)) where
    type SymbolLength' (Just '(c, s)) = 1 + SymbolLength s

    symbolAt _ i
        | i == 0
        = charVal (Proxy @c)

        | otherwise
        = symbolAt (Proxy @(UnconsSymbol s)) (fromIntegral (i - 1))

type SymbolLength s = SymbolLength' (UnconsSymbol s)
type IndexableSymbol s = IndexableSymbol' (UnconsSymbol s)
