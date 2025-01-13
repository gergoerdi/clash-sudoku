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
type Transition s i o = i :- (s, o, Bool)

mapState :: (s -> s') -> Transition s i o -> Transition s' i o
mapState f = fmap \(s, y, consume) -> (f s, y, consume)

class (NFDataX (State fmt)) => Format fmt where
    type State fmt

    start :: fmt -> State fmt
    transition :: fmt -> State fmt -> Transition (Maybe (State fmt)) Word8 (Maybe Word8)
