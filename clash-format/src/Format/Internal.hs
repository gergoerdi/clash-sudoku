{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Format.Internal where

import Clash.Prelude hiding (Const)
import Data.Profunctor

import Data.Word
import Data.Proxy

-- | A @b@ value that potentially depends on an @a@ parameter
data a :- b
    = Const b
    | Varying (a -> b)
    deriving (Functor)
infixr 0 :-

instance Profunctor (:-) where
    dimap to from = \case
        Const x -> Const (from x)
        Varying f -> Varying (from . f . to)

-- | A state transition of a @compander@ with internal state @s@, input @a@ and output @b@
type Transition s i o = i :- (s, o, Bool)

mapState :: (s -> s') -> Transition s i o -> Transition s' i o
mapState f = fmap \(s, y, consume) -> (f s, y, consume)

class (NFDataX (State fmt)) => Format (fmt :: k) where
    type State fmt

    start_ :: proxy fmt -> State fmt
    transition_ :: proxy fmt -> State fmt -> Transition (Maybe (State fmt)) Word8 (Maybe Word8)

start :: forall fmt -> (Format fmt) => State fmt
start fmt = start_ (Proxy @fmt)

transition :: forall fmt -> (Format fmt) => State fmt -> Transition (Maybe (State fmt)) Word8 (Maybe Word8)
transition fmt = transition_ (Proxy @fmt)
