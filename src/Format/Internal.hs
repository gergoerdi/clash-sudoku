{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Format.Internal where

import Clash.Prelude

import Data.Word
import Data.Bifunctor

-- | A @b@ value that potentially depends on an @a@ parameter
data Dep a b
    = Static b
    | Dynamic (a -> b)
    deriving (Functor)

-- | A transition to a new state @s@, potentially consuming the input and potentially producing output @b@
data Transition s b = Transition Bool (Maybe b) s
    deriving (Functor)

instance Bifunctor Transition where
    bimap f g (Transition consume output next) = Transition consume (g <$> output) (f next)

type Format1 a s b = Dep a (Transition (Maybe s) b)

mapState :: (Maybe s -> Maybe s') -> Format1 a s b -> Format1 a s' b
mapState = fmap . first

class (NFDataX (State fmt)) => Format (fmt :: k) where
    type State fmt

    start :: proxy fmt -> State fmt
    format1 :: proxy fmt -> State fmt -> Format1 Word8 (State fmt) Word8
