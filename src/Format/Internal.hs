{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Format.Internal where

import Clash.Prelude

import Data.Word
import Data.Bifunctor
import Data.Proxy

-- | A @b@ value that potentially depends on an @a@ parameter
data Dep a b
    = Static b
    | Dynamic (a -> b)
    deriving (Functor)

-- | A transition to a new state @s@, potentially consuming the input and potentially producing output @b@
data Transition s output = Transition output s
    deriving (Functor)

instance Bifunctor Transition where
    bimap f g (Transition output next) = Transition (g output) (f next)

data Compand b = Compand Bool (Maybe b)
type Format1 a s b = Dep a (Transition (Maybe s) (Compand b))

mapState :: (Maybe s -> Maybe s') -> Format1 a s b -> Format1 a s' b
mapState = fmap . first

class (NFDataX (State fmt)) => Format (fmt :: k) where
    type State fmt

    start_ :: proxy fmt -> State fmt
    format1_ :: proxy fmt -> State fmt -> Format1 Word8 (State fmt) Word8

start :: forall fmt -> (Format fmt) => State fmt
start fmt = start_ (Proxy @fmt)

format1 :: forall fmt -> (Format fmt) => State fmt -> Format1 Word8 (State fmt) Word8
format1 fmt = format1_ (Proxy @fmt)
