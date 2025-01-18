{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module Format.Internal where

import Clash.Prelude hiding (Const)
import Data.Profunctor

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

data Format i o where
    MkFormat :: (NFDataX s) => s -> (s -> Transition (Maybe s) i (Maybe o)) -> Format i o

instance Profunctor Format where
    dimap f g (MkFormat s0 step) = MkFormat s0 (dimap f (\(s', o, consumed) -> (s', g <$> o, consumed)) . step)
