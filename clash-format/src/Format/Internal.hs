{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Format.Internal where

import Clash.Prelude

import Data.Word

-- | A @b@ value that potentially depends on an @a@ parameter
data a :- b
    = Const b
    | Varying (a -> b)
    deriving (Functor)
infixr 0 :-

-- | A state transition of a @compander@ with internal state @s@, input @a@ and output @b@
type Transition s i o = i :- (s, o, Bool)

type FormatTransition s i o = s -> Transition (Maybe s) i (Maybe o)

mapState :: (s -> s') -> Transition s i o -> Transition s' i o
mapState f = fmap \(s, y, consume) -> (f s, y, consume)

class (NFDataX (State fmt)) => FormatState fmt where
    type State fmt
    start :: fmt -> State fmt

class (FormatState fmt) => Format i o fmt where
    transition :: fmt -> FormatTransition (State fmt) i o
