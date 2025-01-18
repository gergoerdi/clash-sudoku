{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Format.Internal where

import Clash.Prelude

import Data.Word
import Data.Proxy

-- | A @b@ value that potentially depends on an @a@ parameter
data a :- b
    = Const b
    | Varying (a -> b)
    deriving (Functor)
infixr 0 :-

-- | A state transition of a @compander@ with internal state @s@, input @a@ and output @b@
type Transition i s o = i :- (s, o, Bool)

mapState :: (s -> s') -> Transition i s o -> Transition i s' o
mapState f = fmap \(s, y, consume) -> (f s, y, consume)

class (NFDataX (State fmt)) => Format (fmt :: k) where
    type State fmt

    start_ :: proxy fmt -> State fmt
    transition_ :: proxy fmt -> State fmt -> Transition Word8 (Maybe (State fmt)) (Maybe Word8)

start :: forall fmt -> (Format fmt) => State fmt
start fmt = start_ (Proxy @fmt)

transition :: forall fmt -> (Format fmt) => State fmt -> Transition Word8 (Maybe (State fmt)) (Maybe Word8)
transition fmt = transition_ (Proxy @fmt)
