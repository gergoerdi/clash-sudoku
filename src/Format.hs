{-# LANGUAGE BlockArguments, TupleSections, LambdaCase #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Format
    ( Format
    , format

    , compander'

    , Print
    , Drop
    , Wait
    , (:*)
    , type (*:)
    , (:++)
    , If
    , Until
    , While
    , Loop

    , ascii
    , asciiVal
    , countSuccChecked
    ) where

import Clash.Prelude hiding (Const)

import Format.Internal
import Format.SymbolAt
import Format.Cond

import Clash.Class.Counter
import Protocols
import qualified Protocols.Df as Df
import Data.Word
import Data.Maybe
import Data.Proxy

compander'
    :: (HiddenClockResetEnable dom, NFDataX s)
    => s
    -> (s -> Transition i s (Maybe o))
    -> Circuit (Df dom i) (Df dom o)
compander' s0 step = Df.compander (s0, True) \(s, ready) x -> case step s of
    Const (s', y, consume) -> ((s', ready && not consume), y, False)
    Varying f
        | ready, (s', y , consume) <- f x -> ((s', not consume), y, False)
        | otherwise -> ((s, True), Nothing, True)

countSuccChecked :: (Counter a) => a -> Maybe a
countSuccChecked = countSucc . Just

-- | Consume one token of input and forward it to the output
data Print

instance FormatState Print where
    type State Print = ()
    start_ _ = ()

instance Format a a Print where
    transition_ _ _ = Varying \x -> (Nothing, Just x, True)

-- | Consume one token of input without producing any output
data Drop

instance FormatState Drop where
    type State Drop = ()
    start_ _ = ()

instance Format a b Drop where
    transition_ _ _ = Varying \x -> (Nothing, Nothing, True)

-- | Wait until new input is available, without consuming it
data Wait

instance FormatState Wait where
    type State Wait = ()
    start_ _ = ()

instance Format a b Wait where
    transition_ _ _ = Varying \x -> (Nothing, Nothing, False)

-- | Repetition
infix 7 :*
data a :* (n :: Nat)

infix 7 *:
type n *: fmt = fmt :* n

instance (FormatState fmt, KnownNat n, 1 <= n) => FormatState (fmt :* n) where
    type State (fmt :* n) = (State fmt, Index n)
    start_ _ = (start fmt, 0)

instance (Format a b fmt, KnownNat n, 1 <= n) => Format a b (fmt :* n) where
    transition_ _ (s, i) = mapState (maybe repeat continue) $ transition fmt s
      where
        continue s' = Just (s', i)
        repeat = (start fmt,) <$> countSuccChecked i

-- | Concatenation
infixl 6 :++
data a :++ b

instance (FormatState fmt1, FormatState fmt2) => FormatState (fmt1 :++ fmt2) where
    type State (fmt1 :++ fmt2) = Either (State fmt1) (State fmt2)
    start_ _ = Left (start fmt1)

instance (Format a b fmt1, Format a b fmt2) => Format a b (fmt1 :++ fmt2) where
    transition_ _ = either
      (mapState (Just . maybe (Right $ start fmt2) Left) . transition fmt1)
      (mapState (fmap Right) . transition fmt2)

-- | Character literal
instance (KnownChar ch) => FormatState ch where
    type State ch = ()
    start_ _ = ()

instance (KnownChar ch) => Format a Char ch where
    transition_ _ _ = Const (Nothing, Just $ charVal (Proxy @ch), False)

-- | String literal
instance (SymbolVec str, KnownNat (SymbolLength str), 1 <= SymbolLength str) => FormatState str where
    type State str = Index (SymbolLength str)
    start_ _ = 0

instance (SymbolVec str, KnownNat (SymbolLength str), 1 <= SymbolLength str) => Format a Char str where
    transition_ _ = \i -> Const (countSuccChecked i, Just (s !! i), False)
      where
        s = symbolVec str

-- | Loop
data Loop fmt

instance (FormatState fmt) => FormatState (Loop fmt) where
    type State (Loop fmt) = State fmt
    start_ _ = start fmt

instance (Format a b fmt) => Format a b (Loop fmt) where
    transition_ _ = mapState (Just . fromMaybe (start fmt)) . transition fmt

-- | Branch
data If c thn els

data IfState thn els
    = Decide
    | Then thn
    | Else els
    deriving (Generic, NFDataX, Show)

branch :: (i -> s) -> Transition i s (Maybe o)
branch p = Varying \x -> (p x, Nothing, False)

instance (FormatState thn, FormatState els) => FormatState (If c thn els) where
    type State (If c thn els) = IfState (State thn) (State els)
    start_ _ = Decide

instance (Cond a c, Format a b thn, Format a b els) => Format a b (If c thn els) where
    transition_ _ = \case
        Decide -> branch \x -> Just $ if cond c x then Then (start thn) else Else (start els)
        Then s -> mapState (Then <$>) $ transition thn s
        Else s -> mapState (Else <$>) $ transition els s

-- | Branch
data Until c fmt

data UntilState fmt
    = Checking
    | Looping fmt
    deriving (Generic, NFDataX, Show)

instance (FormatState fmt) => FormatState (Until c fmt) where
    type State (Until c fmt) = UntilState (State fmt)
    start_ _ = Checking

instance (Cond a c, Format a b fmt) => Format a b (Until c fmt) where
    transition_ _ = \case
        Checking -> branch \x -> if cond c x then Nothing else Just $ Looping $ start fmt
        Looping s -> mapState (Just . maybe Checking Looping) $ transition fmt s

type While c = Until (Not c)

{-# INLINE format #-}
format
    :: forall dom a b. (HiddenClockResetEnable dom)
    => forall fmt -> (Format a b fmt)
    => Circuit (Df dom a) (Df dom b)
format fmt = compander' (Just $ start fmt) \case
    Nothing -> Varying \_ -> (Nothing, Nothing, True)
    Just s -> transition fmt s
