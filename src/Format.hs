{-# LANGUAGE BlockArguments, TupleSections, LambdaCase #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments, StandaloneDeriving #-}
module Format
    ( Format
    , format

    , Forward
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

import Clash.Prelude

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

countSuccChecked :: (Counter a) => a -> Maybe a
countSuccChecked = countSucc . Just

-- | Consume one token of input and forward it to the output
data Forward

instance Format Forward where
    type State Forward = ()

    start_ _ = ()
    transition_ _ _ = Dynamic \x -> Step True (Just x) Nothing

-- | Consume one token of input without producing any output
data Drop

instance Format Drop where
    type State Drop = ()

    start_ _ = ()
    transition_ _ _ = Dynamic \x -> Step True Nothing Nothing

-- | Wait until new input is available, without consuming it
data Wait

instance Format Wait where
    type State Wait = ()

    start_ _ = ()
    transition_ _ _ = Dynamic \x -> Step False Nothing Nothing

-- | Repetition
infix 7 :*
data a :* (rep :: Nat)

infix 7 *:
type n *: fmt = fmt :* n

instance (Format fmt, KnownNat rep, 1 <= rep) => Format (fmt :* rep) where
    type State (fmt :* rep) = (Index rep, State fmt)

    start_ _ = (countMin, start fmt)

    transition_ _ (i, s) = mapState (maybe repeat continue) $ transition fmt s
      where
        continue s' = Just (i, s')
        repeat = (, start fmt) <$> countSuccChecked i

-- | Concatenation
infixl 6 :++
data a :++ b

instance (Format a, Format b) => Format (a :++ b) where
    type State (a :++ b) = Either (State a) (State b)

    start_ _ = Left (start a)

    transition_ _ = either
      (mapState (Just . maybe (Right $ start b) Left) . transition a)
      (mapState (fmap Right) . transition b)

-- | Literal
instance (IndexableSymbol sep, KnownNat (SymbolLength sep), 1 <= SymbolLength sep) => Format sep where
    type State sep = Index (SymbolLength sep)

    start_ _ = countMin

    transition_ _ i = Static $ Step False (Just char) (countSuccChecked i)
      where
        char = ascii $ noDeDup $ symbolAt sep i

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

branch :: (a -> Maybe s) -> Transition a s b
branch p = Dynamic \x -> Step False Nothing $ p x

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
format fmt = Df.compander (Just $ start fmt, True) \(s, ready) x ->
    let wait_for_next_input = ((s, True), Nothing, True)
        proceed (Step consume y s') = ((s', ready && not consume), y, False)
    in case s of
        Nothing -> wait_for_next_input
        Just s -> case transition fmt s of
            Static step -> proceed step
            Dynamic f | ready -> proceed (f x)
            _ -> wait_for_next_input
