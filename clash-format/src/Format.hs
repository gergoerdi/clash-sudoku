{-# LANGUAGE BlockArguments, TupleSections, LambdaCase #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments, RankNTypes #-}
module Format
    ( Format
    , format

    , compander'

    , Print(..)
    , Drop(..)
    , Wait(..)
    , Lit(..), Str, str
    , Map(..)
    , (:*), (*:)
    , (:++)(..)
    , If(..)
    , Until(..)
    , while
    , Loop(..)

    , ascii
    , asciiVal
    , countSuccChecked
    ) where

import Clash.Prelude hiding (Const)

import Format.Internal
import Format.SymbolAt

import Clash.Class.Counter
import Protocols
import qualified Protocols.Df as Df
import Data.Maybe

compander'
    :: (HiddenClockResetEnable dom, NFDataX s)
    => s
    -> (s -> Transition s i (Maybe o))
    -> Circuit (Df dom i) (Df dom o)
compander' s0 step = Df.compander (s0, True) \(s, ready) x -> case step s of
    Const (s', y, consume) -> ((s', ready && not consume), y, False)
    Varying f
        | ready, (s', y , consume) <- f x -> ((s', not consume), y, False)
        | otherwise -> ((s, True), Nothing, True)

countSuccChecked :: (Counter a) => a -> Maybe a
countSuccChecked = countSucc . Just

-- | Consume one token of input and forward it to the output
data Print = Print

instance FormatState Print where
    type State Print = ()
    start _ = ()

instance (i ~ o) => Format i o Print where
    transition _ _ = Varying \x -> (Nothing, Just x, True)

-- | Consume one token of input without producing any output
data Drop = Drop

instance FormatState Drop where
    type State Drop = ()
    start _ = ()

instance Format i o Drop where
    transition _ _ = Varying \x -> (Nothing, Nothing, True)

-- | Wait until new input is available, without consuming it
data Wait = Wait

instance FormatState Wait where
    type State Wait = ()
    start _ = ()

instance Format i o Wait where
    transition _ _ = Varying \x -> (Nothing, Nothing, False)

-- | Repetition
infix 7 :*
data (n :: Nat) :* a = Rep a

(*:) :: forall (n :: Nat) -> fmt -> n :* fmt
n *: fmt = Rep fmt

instance (FormatState fmt, KnownNat n, 1 <= n) => FormatState (n :* fmt) where
    type State (n :* fmt) = (State fmt, Index n)
    start (Rep fmt) = (start fmt, 0)

instance (Format i o fmt, KnownNat n, 1 <= n) => Format i o (n :* fmt) where
    transition (Rep fmt) (s, i) = mapState (maybe repeat continue) $ transition fmt s
      where
        continue s' = Just (s', i)
        repeat = (start fmt,) <$> countSuccChecked i

-- | Concatenation
infixl 6 :++
data a :++ b = a :++ b

instance (FormatState fmt1, FormatState fmt2) => FormatState (fmt1 :++ fmt2) where
    type State (fmt1 :++ fmt2) = Either (State fmt1) (State fmt2)
    start (fmt1 :++ fmt2) = Left (start fmt1)

instance (Format i o fmt1, Format i o fmt2) => Format i o (fmt1 :++ fmt2) where
    transition (fmt1 :++ fmt2) = either
      (mapState (Just . maybe (Right $ start fmt2) Left) . transition fmt1)
      (mapState (fmap Right) . transition fmt2)

-- | Literal
data Lit o = Lit o

instance FormatState (Lit o) where
    type State (Lit o) = ()

    start _ = ()

instance Format i o (Lit o) where
    transition (Lit x) _ = Const (Nothing, Just x, False)

-- | String literal
data Str (sym :: Symbol) = Str

str :: forall (sym :: Symbol) -> Str sym
str _ = Str

instance (SymbolVec sym, KnownNat (SymbolLength sym), 1 <= SymbolLength sym) => FormatState (Str sym) where
    type State (Str sym) = Index (SymbolLength sym)
    start _ = 0

instance (o ~ Char, SymbolVec sym, KnownNat (SymbolLength sym), 1 <= SymbolLength sym) => Format i o (Str sym) where
    transition _ = \i -> Const (countSuccChecked i, Just (s !! i), False)
      where
        s = symbolVec sym

-- | Loop
data Loop fmt = Loop fmt

instance (FormatState fmt) => FormatState (Loop fmt) where
    type State (Loop fmt) = State fmt
    start (Loop fmt) = start fmt

instance (Format i o fmt) => Format i o (Loop fmt) where
    transition (Loop fmt) = mapState (Just . fromMaybe (start fmt)) . transition fmt

-- | Branch
data If i thn els = If (i -> Bool) thn els

data IfState thn els
    = Decide
    | Then thn
    | Else els
    deriving (Generic, NFDataX, Show)

branch :: (i -> s) -> Transition s i (Maybe o)
branch p = Varying \x -> (p x, Nothing, False)

instance (FormatState thn, FormatState els) => FormatState (If i thn els) where
    type State (If i thn els) = IfState (State thn) (State els)
    start _ = Decide

instance (Format i o thn, Format i o els) => Format i o (If i thn els) where
    transition (If cond thn els) = \case
        Decide -> branch \x -> Just $ if cond x then Then (start thn) else Else (start els)
        Then s -> mapState (Then <$>) $ transition thn s
        Else s -> mapState (Else <$>) $ transition els s

-- | Branch
data Until i fmt = Until (i -> Bool) fmt

data UntilState fmt
    = Checking
    | Looping fmt
    deriving (Generic, NFDataX, Show)

instance (FormatState fmt) => FormatState (Until i fmt) where
    type State (Until i fmt) = UntilState (State fmt)
    start _ = Checking

instance (Format i o fmt) => Format i o (Until i fmt) where
    transition (Until cond fmt) = \case
        Checking -> branch \x -> if cond x then Nothing else Just $ Looping $ start fmt
        Looping s -> mapState (Just . maybe Checking Looping) $ transition fmt s

while = Until . (not . )

data Map a b fmt = Map (a -> b) fmt

instance (FormatState fmt) => FormatState (Map a b fmt) where
    type State (Map a b fmt) = State fmt
    start (Map f fmt) = start fmt

instance (Format i a fmt) => Format i b (Map a b fmt) where
    transition (Map f fmt) = fmap (\(s, o, c) -> (s, f <$> o, c)) . transition fmt

{-# INLINE format #-}
format
    :: forall i o dom fmt. (HiddenClockResetEnable dom, Format i o fmt)
    => fmt -> Circuit (Df dom i) (Df dom o)
format fmt = compander' (Just $ start fmt) \case
    Nothing -> Varying \_ -> (Nothing, Nothing, True)
    Just s -> transition fmt s
