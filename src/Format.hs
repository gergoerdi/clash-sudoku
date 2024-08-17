{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Format
    ( ascii
    , Forward
    , Drop
    , Wait
    , (:*)
    , (:++)
    , Until
    , Loop
    , format
    , formatModel
    , countSuccChecked
    ) where

import Format.SymbolAt

import Clash.Prelude
import Clash.Class.Counter
import Clash.Class.Counter.Internal
import Protocols
import qualified Protocols.Df as Df
import Data.Proxy
import Data.Char (ord)
import Data.Word
import qualified Data.List as L
import Control.Monad (guard)
import Data.Maybe
import Data.Bifunctor

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

ascii :: Char -> Word8
ascii c
    | code <= 0x7f = fromIntegral code
    | otherwise = clashCompileError "Not an ASCII code point"
  where
    code = ord c

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

countSuccChecked :: Counter a => a -> Maybe a
countSuccChecked x = x' <$ guard (not overflow)
  where
    (overflow, x') = countSuccOverflow x

class (NFDataX (State fmt)) => Format (fmt :: k) where
    type State fmt

    start :: proxy fmt -> State fmt
    format1 :: proxy fmt -> State fmt -> Format1 Word8 (State fmt) Word8

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

formatModel :: forall fmt a. (Format fmt) => Proxy fmt -> [Word8] -> [Word8]
formatModel fmt = go begin
  where
    begin = start fmt

    go s cs = case format1 fmt s of
        Static step -> produce step
        Dynamic step
            | (c:_) <- cs -> produce (step c)
            | otherwise -> []
      where
        next = fromMaybe begin
        output = maybe id (:)
        produce (Transition consume mb_y s') = output mb_y $ go (next s') $ if consume then L.tail cs else cs

prop_format :: (Format fmt) => Proxy fmt -> H.Property
prop_format fmt =
    H.idWithModelSingleDomain
      H.defExpectOptions
      gen_input
      (\_ _ _ -> formatModel fmt)
      (exposeClockResetEnable @System $ format fmt)
  where
    gen_input :: H.Gen [Word8]
    gen_input = Gen.list (Range.linear 0 100) (ascii <$> Gen.alpha)
