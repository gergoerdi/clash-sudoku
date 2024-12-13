{-# LANGUAGE PolyKinds, RequiredTypeArguments #-}
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

class Cond c where
    cond_ :: proxy c -> Word8 -> Bool

cond :: forall c -> (Cond c) => Word8 -> Bool
cond c = cond_ (Proxy @c)

instance (KnownChar ch) => Cond ch where
    cond_ _ = (== asciiVal ch)

data Not cond

instance (Cond c) => Cond (Not c) where
    cond_ _ = not . cond c
