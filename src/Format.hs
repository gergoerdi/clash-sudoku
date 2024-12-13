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
    , type (*:)
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
import Protocols
import qualified Protocols.Df as Df
import Data.Proxy
import Data.Char (ord)
import Data.Word
import Data.Maybe

ascii :: Char -> Word8
ascii c
    | code <= 0x7f = fromIntegral code
    | otherwise = clashCompileError "Not an ASCII code point"
  where
    code = ord c

countSuccChecked :: (Counter a) => a -> Maybe a
countSuccChecked = countSucc . Just

-- | Consume one token of input and forward it to the output
data Forward

instance Format Forward where
    type State Forward = ()

    start_ _ = ()

    format1_ _ _ = Dynamic \x -> Step True (Just x) Nothing

-- | Consume one token of input without producing any output
data Drop

instance Format Drop where
    type State Drop = ()

    start_ _ = ()
    format1_ _ _ = Dynamic \x -> Step True Nothing Nothing

-- | Wait until new input is available, without consuming it
data Wait

instance Format Wait where
    type State Wait = ()

    start_ _ = ()
    format1_ _ _ = Dynamic \x -> Step False Nothing Nothing

-- | Repetition
infix 7 :*
data a :* (rep :: Nat)

infix 7 *:
type n *: fmt = fmt :* n

instance (Format fmt, KnownNat rep, 1 <= rep) => Format (fmt :* rep) where
    type State (fmt :* rep) = (Index rep, State fmt)

    start_ _ = (countMin, start fmt)

    format1_ _ (i, s) = mapState (maybe repeat continue) $ format1 fmt s
      where
        continue s' = Just (i, s')
        repeat = (, start fmt) <$> countSuccChecked i

-- | Concatenation
infix 6 :++
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

    format1_ _ i = Static $ Step False (Just char) (countSuccChecked i)
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
                Step True Nothing (Just $ Then $ start thn)
            else
                Step False Nothing (Just $ Else $ start els)
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
                Step True Nothing Nothing
            else
                Step False Nothing (Just $ Looping $ start fmt)
        Looping s ->
            mapState (Just . maybe Checking Looping) $ format1 fmt s
      where
        ch = charVal (Proxy @ch)

{-# INLINE format #-}
format
    :: forall dom. (HiddenClockResetEnable dom)
    => forall fmt -> (Format fmt)
    => Circuit (Df dom Word8) (Df dom Word8)
format fmt = Df.compander (start fmt, True) \(s, ready) x ->
    let wait_for_next_input = ((s, True), Nothing, True)
        transition (Step consume y s') = ((fromMaybe (start fmt) s', ready && not consume), y, False)
    in case format1 fmt s of
        Static step -> transition step
        Dynamic f | ready -> transition (f x)
        _ -> wait_for_next_input
