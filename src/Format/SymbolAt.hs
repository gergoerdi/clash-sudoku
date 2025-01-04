{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Format.SymbolAt (SymbolLength, SymbolVec, symbolVec) where

import Clash.Prelude
import Data.Proxy
import Format.Cond
import Data.Word

class (KnownNat (SymbolLength' s)) => SymbolVec' (s :: Maybe (Char, Symbol)) where
    type SymbolLength' s :: Nat

    symbolVec' :: Proxy s -> Vec (SymbolLength' s) Char

instance SymbolVec' Nothing where
    type SymbolLength' Nothing = 0

    symbolVec' _ = Nil

instance (SymbolVec s, KnownChar c) => SymbolVec' (Just '(c, s)) where
    type SymbolLength' (Just '(c, s)) = 1 + SymbolLength s

    symbolVec' _ = charVal (Proxy @c) :> symbolVec s

type SymbolLength s = SymbolLength' (UnconsSymbol s)
type SymbolVec s = SymbolVec' (UnconsSymbol s)

symbolVec :: forall s -> SymbolVec s => Vec (SymbolLength s) Char
symbolVec s = symbolVec' (Proxy @(UnconsSymbol s))
