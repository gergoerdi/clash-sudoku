{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Format
    ( Format
    , format

    , Forward
    , Drop
    , Wait
    , (:*)
    , (:++)
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

    start _ = ()

    format1 _ _ = Dynamic \x -> Transition True (Just x) Nothing

-- | Consume one token of input without producing any output
data Drop

instance Format Drop where
    type State Drop = ()

    start _ = ()
    format1 _ _ = Dynamic \x -> Transition True Nothing Nothing

-- | Wait until new input is available, without consuming it
data Wait

instance Format Wait where
    type State Wait = ()

    start _ = ()
    format1 _ _ = Dynamic \x -> Transition False Nothing Nothing

-- | Repetition
data a :* (rep :: Nat)

instance (Format fmt, KnownNat rep, 1 <= rep) => Format (fmt :* rep) where
    type State (fmt :* rep) = (Index rep, State fmt)

    start _ = (countMin, start (Proxy @fmt))

    format1 _ (i, s) = mapState (maybe repeat continue) $ format1 (Proxy @fmt) s
      where
        continue s' = Just (i, s')
        repeat = (, start (Proxy @fmt)) <$> countSuccChecked i

-- | Concatenation
data a :++ b

instance (Format a, Format b) => Format (a :++ b) where
    type State (a :++ b) = Either (State a) (State b)

    start _ = Left (start (Proxy @a))

    format1 _ = either
      (mapState (maybe (Just . Right $ start (Proxy @b)) (Just . Left)) . format1 (Proxy @a))
      (mapState (maybe Nothing (Just . Right)) . format1 (Proxy @b))

-- | Literal
instance (IndexableSymbol sep, KnownNat (SymbolLength sep), 1 <= SymbolLength sep) => Format sep where
    type State sep = Index (SymbolLength sep)

    start _ = countMin

    format1 _ i = Static $ Transition False (Just char) (countSuccChecked i)
      where
        char = ascii $ noDeDup $ symbolAt (Proxy @sep) i

-- | Loop
data Loop fmt

instance (Format fmt) => Format (Loop fmt) where
    type State (Loop fmt) = State fmt

    start _ = start (Proxy @fmt)
    format1 _ = mapState (Just . fromMaybe (start (Proxy @fmt))) . format1 (Proxy @fmt)

-- | Branch
data Until (ch :: Char) fmt

data UntilState fmt
    = Checking
    | Looping fmt
    deriving (Generic, NFDataX, Show)

instance (KnownChar ch, Format fmt) => Format (Until ch fmt) where
    type State (Until ch fmt) = UntilState (State fmt)

    start _ = Checking

    format1 _ Checking = Dynamic \x ->
        if x == ascii ch then
            Transition True Nothing Nothing
        else
            Transition False Nothing (Just enter)
      where
        enter = Looping $ start (Proxy @fmt)
        ch = charVal (Proxy @ch)
    format1 _ (Looping s) = mapState (Just . maybe Checking Looping) $ format1 (Proxy @fmt) s

{-# INLINE format #-}
format
    :: forall dom fmt. (HiddenClockResetEnable dom, Format fmt)
    => Proxy fmt
    -> Circuit (Df dom Word8) (Df dom Word8)
format fmt = Df.compander (begin, True) \(s, ready) x ->
    let next_input = ((s, True), Nothing, True)
        produce (Transition consume y s') = ((fromMaybe begin s', ready && not consume), y, False)
    in case format1 fmt s of
        Dynamic step | not ready -> next_input
        Static step -> produce step
        Dynamic step -> produce (step x)
  where
    begin = start fmt
