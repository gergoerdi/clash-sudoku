{-# LANGUAGE BlockArguments, TupleSections, LambdaCase #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments, StandaloneDeriving #-}
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
import Data.Proxy
import Data.Char (ord)
import Data.Word
import Data.Maybe

compander'
    :: (HiddenClockResetEnable dom, NFDataX s)
    => s
    -> (s -> i :- (s, Maybe o, Bool))
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

instance Format Print where
    type State Print = ()

    start_ _ = ()
    transition_ _ _ = Varying \x -> (Nothing, Just x, True)

-- | Consume one token of input without producing any output
data Drop

instance Format Drop where
    type State Drop = ()

    start_ _ = ()
    transition_ _ _ = Varying \x -> (Nothing, Nothing, True)

-- | Wait until new input is available, without consuming it
data Wait

instance Format Wait where
    type State Wait = ()

    start_ _ = ()
    transition_ _ _ = Varying \x -> (Nothing, Nothing, False)

-- | Repetition
infix 7 :*
data a :* (n :: Nat)

infix 7 *:
type n *: fmt = fmt :* n

instance (Format fmt, KnownNat n, 1 <= n) => Format (fmt :* n) where
    type State (fmt :* n) = (State fmt, Index n)

    start_ _ = (start fmt, 0)

    transition_ _ (s, i) = mapState (maybe repeat continue) $ transition fmt s
      where
        continue s' = Just (s', i)
        repeat = (start fmt,) <$> countSuccChecked i

-- | Concatenation
infixl 6 :++
data a :++ b

instance (Format a, Format b) => Format (a :++ b) where
    type State (a :++ b) = Either (State a) (State b)

    start_ _ = Left (start a)

    transition_ _ = either
      (mapState (Just . maybe (Right $ start b) Left) . transition a)
      (mapState (fmap Right) . transition b)

-- | Character literal
instance (KnownChar ch) => Format ch where
    type State ch = ()

    start_ _ = ()
    transition_ _ _ = Const (Nothing, Just $ asciiVal ch, False)

-- | String literal
instance (IndexableSymbol str, KnownNat (SymbolLength str), 1 <= SymbolLength str) => Format str where
    type State str = Index (SymbolLength str)

    start_ _ = countMin

    transition_ _ i = Const (countSuccChecked i, Just char, False)
      where
        char = ascii $ noDeDup $ symbolAt str i

-- | Loop
data Loop fmt

instance (Format fmt) => Format (Loop fmt) where
    type State (Loop fmt) = State fmt

    start_ _ = start fmt
    transition_ _ = mapState (Just . fromMaybe (start fmt)) . transition fmt

-- | Branch
data If c thn els

data IfState thn els
    = Decide
    | Then (State thn)
    | Else (State els)
    deriving (Generic)

deriving instance (NFDataX (State thn), NFDataX (State els)) => NFDataX (IfState thn els)
deriving instance (Show (State thn), Show (State els)) => Show (IfState thn els)

branch :: (a -> s) -> Transition a s b
branch p = Varying \x -> (p x, Nothing, False)

instance (Cond c, Format thn, Format els) => Format (If c thn els) where
    type State (If c thn els) = IfState thn els

    start_ _ = Decide

    transition_ _ = \case
        Decide -> branch \x -> Just $ if cond c x then Then (start thn) else Else (start els)
        Then s -> mapState (Then <$>) $ transition thn s
        Else s -> mapState (Else <$>) $ transition els s

-- | Branch
data Until c fmt

data UntilState fmt
    = Checking
    | Looping (State fmt)
    deriving (Generic)

deriving instance (NFDataX (State fmt)) => NFDataX (UntilState fmt)
deriving instance (Show (State fmt)) => Show (UntilState fmt)

instance (Cond c, Format fmt) => Format (Until c fmt) where
    type State (Until c fmt) = UntilState fmt

    start_ _ = Checking
    transition_ _ = \case
        Checking -> branch \x -> if cond c x then Nothing else Just $ Looping $ start fmt
        Looping s -> mapState (Just . maybe Checking Looping) $ transition fmt s

type While c = Until (Not c)

{-# INLINE format #-}
format
    :: forall dom. (HiddenClockResetEnable dom)
    => forall fmt -> (Format fmt)
    => Circuit (Df dom Word8) (Df dom Word8)
format fmt = compander' (Just $ start fmt) \case
    Nothing -> Varying \_ -> (Nothing, Nothing, True)
    Just s -> transition fmt s
