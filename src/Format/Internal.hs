{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Format.Internal where

import Clash.Prelude

import Data.Word
import Data.Proxy

-- | A @b@ value that potentially depends on an @a@ parameter
data a :~> b
    = Static b
    | Dynamic (a -> b)
    deriving (Functor)
infixr 0 :~>

data Step b s = Step
    Bool -- ^ Consume input?
    (Maybe b) -- ^ Produce output?
    s -- ^ Next state
    deriving (Functor)

-- | A state transition of a @compander@ with internal state @s@, input @a@ and output @b@
type Transition a s b = a :~> Step b (Maybe s)

mapState :: (Maybe s -> Maybe s') -> Transition a s b -> Transition a s' b
mapState = fmap . fmap

class (NFDataX (State fmt)) => Format (fmt :: k) where
    type State fmt

    start_ :: proxy fmt -> State fmt
    transition_ :: proxy fmt -> State fmt -> Transition Word8 (State fmt) Word8

start :: forall fmt -> (Format fmt) => State fmt
start fmt = start_ (Proxy @fmt)

transition :: forall fmt -> (Format fmt) => State fmt -> Transition Word8 (State fmt) Word8
transition fmt = transition_ (Proxy @fmt)
