{-# LANGUAGE BlockArguments, TupleSections, LambdaCase #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Format
    ( Format
    , format

    , Forward
    , Drop
    , Wait
    , (:*)
    , (:++)
    , If
    , Until
    , Loop

    , ascii
    , countSuccChecked
    ) where

import Clash.Prelude

import Format.Internal
import Format.SymbolAt

import Clash.Class.Counter
import Clash.Class.Counter.Internal
import Protocols
import qualified Protocols.Df as Df
import Data.Proxy
import Data.Char (ord)
import Data.Word
import Control.Monad (guard)
import Data.Maybe

ascii :: Char -> Word8
ascii c
    | code <= 0x7f = fromIntegral code
    | otherwise = clashCompileError "Not an ASCII code point"
  where
    code = ord c

countSuccChecked :: Counter a => a -> Maybe a
countSuccChecked x = x' <$ guard (not overflow)
  where
    (overflow, x') = countSuccOverflow x

-- | Consume one token of input and forward it to the output
data Forward

instance Format Forward where
    type State Forward = ()

    start_ _ = ()

    format1_ _ _ = Dynamic \x -> Transition True (Just x) Nothing

-- | Consume one token of input without producing any output
data Drop

instance Format Drop where
    type State Drop = ()

    start_ _ = ()
    format1_ _ _ = Dynamic \x -> Transition True Nothing Nothing

-- | Wait until new input is available, without consuming it
data Wait

instance Format Wait where
    type State Wait = ()

    start_ _ = ()
    format1_ _ _ = Dynamic \x -> Transition False Nothing Nothing

-- | Repetition
data a :* (rep :: Nat)

instance (Format fmt, KnownNat rep, 1 <= rep) => Format (fmt :* rep) where
    type State (fmt :* rep) = (Index rep, State fmt)

    start_ _ = (countMin, start fmt)

    format1_ _ (i, s) = mapState (maybe repeat continue) $ format1 fmt s
      where
        continue s' = Just (i, s')
        repeat = (, start fmt) <$> countSuccChecked i

-- | Concatenation
data a :++ b

instance (Format a, Format b) => Format (a :++ b) where
    type State (a :++ b) = Either (State a) (State b)

    start_ _ = Left (start a)

    format1_ _ = either
      (mapState (maybe (Just . Right $ start b) (Just . Left)) . format1 a)
      (mapState (maybe Nothing (Just . Right)) . format1 b)

-- | Literal
instance (IndexableSymbol sep, KnownNat (SymbolLength sep), 1 <= SymbolLength sep) => Format sep where
    type State sep = Index (SymbolLength sep)

    start_ _ = countMin

    format1_ _ i = Static $ Transition False (Just char) (countSuccChecked i)
      where
        char = ascii $ noDeDup $ symbolAt sep i

-- | Loop
data Loop fmt

instance (Format fmt) => Format (Loop fmt) where
    type State (Loop fmt) = State fmt

    start_ _ = start fmt
    format1_ _ = mapState (Just . fromMaybe (start fmt)) . format1 fmt

-- | Branch
data If (ch :: Char) thn els

data IfState thn els
    = Decide
    | Then thn
    | Else els
    deriving (Generic, NFDataX, Show)

instance (KnownChar ch, Format thn, Format els) => Format (If ch thn els) where
    type State (If ch thn els) = IfState (State thn) (State els)

    start_ _ = Decide

    format1_ _ = \case
        Decide -> Dynamic \x ->
            if x == ascii ch then
                Transition True Nothing (Just $ Then $ start thn)
            else
                Transition False Nothing (Just $ Else $ start els)
        Then s -> mapState (Then <$>) $ format1 thn s
        Else s -> mapState (Else <$>) $ format1 els s
      where
        ch = charVal (Proxy @ch)

-- | Branch
data Until (ch :: Char) fmt

data UntilState fmt
    = Checking
    | Looping fmt
    deriving (Generic, NFDataX, Show)

instance (KnownChar ch, Format fmt) => Format (Until ch fmt) where
    type State (Until ch fmt) = UntilState (State fmt)

    start_ _ = Checking

    format1_ _ = \case
        Checking -> Dynamic \x ->
            if x == ascii ch then
                Transition True Nothing Nothing
            else
                Transition False Nothing (Just $ Looping $ start fmt)
        Looping s ->
            mapState (Just . maybe Checking Looping) $ format1 fmt s
      where
        ch = charVal (Proxy @ch)

{-# INLINE format #-}
format
    :: forall dom. (HiddenClockResetEnable dom)
    => forall fmt -> (Format fmt)
    => Circuit (Df dom Word8) (Df dom Word8)
format fmt = Df.compander (begin, True) \(s, ready) x ->
    let next_input = ((s, True), Nothing, True)
        produce (Transition consume y s') = ((fromMaybe begin s', ready && not consume), y, False)
    in case format1 fmt s of
        Dynamic step | not ready -> next_input
        Static step -> produce step
        Dynamic step -> produce (step x)
  where
    begin = start fmt
