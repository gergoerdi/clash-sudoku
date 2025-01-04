{-# LANGUAGE PolyKinds, RequiredTypeArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Format.Cond
    ( Cond(..)
    , cond

    , Not

    , ascii
    , asciiVal
    ) where

import Clash.Prelude

import Data.Proxy
import Data.Char (ord)
import Data.Word

ascii :: Char -> Word8
ascii c
    | code <= 0x7f = fromIntegral code
    | otherwise = clashCompileError "Not an ASCII code point"
  where
    code = ord c

asciiVal :: forall ch -> (KnownChar ch) => Word8
asciiVal ch = ascii $ charVal (Proxy @ch)

class Cond a c where
    cond_ :: proxy c -> a -> Bool

cond :: forall c -> (Cond a c) => a -> Bool
cond c = cond_ (Proxy @c)

-- | Matches the input exactly
instance (KnownChar ch) => Cond Word8 ch where
    cond_ _ = (== asciiVal ch)

-- | Matches the input exactly
instance (KnownChar ch) => Cond Char ch where
    cond_ _ = (== charVal (Proxy @ch))

-- | Matches if any of the characters match the input
instance (KnownSymbol s) => Cond Char s where
    cond_ _ = (`elem` cs)
      where
        cs = symbolVal (Proxy @s)

-- | Matches if any of the characters match the input
instance (KnownSymbol s) => Cond Word8 s where
    cond_ _ = (`elem` cs)
      where
        cs = fmap ascii $ symbolVal (Proxy @s)

data Not cond

instance (Cond a c) => Cond a (Not c) where
    cond_ _ = not . cond c
