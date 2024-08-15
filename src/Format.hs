{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
module Format
    ( ascii
    , countSuccChecked
    , Forward
    , (:*)
    , (:++)
    , format
    , formatModel
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
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

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

countSuccChecked :: Counter a => a -> Maybe a
countSuccChecked x = x' <$ guard (not overflow)
  where
    (overflow, x') = countSuccOverflow x

data Stateful s a = a :~> Maybe s
    deriving (Generic, NFDataX)

data Format1 s a b
    = Punctuate (Stateful s b)
    | Consume (a -> Stateful s (Maybe b))
    deriving (Generic, NFDataX)

mapState :: (Maybe s -> Maybe s') -> Format1 s a b -> Format1 s' a b
mapState f = \case
    Punctuate (y :~> s) -> Punctuate $ y :~> f s
    Consume step -> Consume $ \x -> case step x of
        y :~> s -> y :~> f s

class (Counter (State fmt a b), NFDataX (State fmt a b)) => Format a b (fmt :: k) where
    type State fmt a b

    format1 :: proxy fmt -> State fmt a b -> Format1 (State fmt a b) a b

-- | Consume one token of input without producing any output
data Drop

instance Format a b Drop where
    type State Drop a b = Index 1

    format1 _ s = Consume (const $ Nothing :~> countSuccChecked s)

-- | Consume one token of input and forward it to the output
data Forward

instance Format a a Forward where
    type State Forward a a = Index 1

    format1 _ s = Consume \x -> Just x :~> countSuccChecked s

-- | Repetition
data a :* (rep :: Nat)

instance (Format a b fmt, KnownNat rep, 1 <= rep) => Format a b (fmt :* rep) where
    type State (fmt :* rep) a b = (Index rep, State fmt a b)

    format1 _ (i, s) = mapState after $ format1 (Proxy @fmt) s
      where
        after :: Maybe (State fmt a b) -> Maybe (Index rep, State fmt a b)
        after = \case
            Nothing -> (, countMin) <$> countSuccChecked i
            Just s' -> Just (i, s')

-- | Concatenation
data a :++ b

instance (Format a b fmt1, Format a b fmt2) => Format a b (fmt1 :++ fmt2) where
    type State (fmt1 :++ fmt2) a b = Either (State fmt1 a b) (State fmt2 a b)

    format1 _ = either
      (mapState (Just . maybe (Right countMin) Left) . format1 (Proxy @fmt1))
      (mapState (maybe Nothing (Just . Right)) . format1 (Proxy @fmt2))

-- | Literal
instance (IndexableSymbol symbol, KnownNat (SymbolLength symbol), 1 <= SymbolLength symbol) => Format a Word8 symbol where
    type State symbol a Word8 = Index (SymbolLength symbol)

    format1 _ i = Punctuate $ ascii (noDeDup $ symbolAt (Proxy @(UnconsSymbol symbol)) i) :~> countSuccChecked i

{-# INLINE format #-}
format
    :: forall dom fmt a b. (HiddenClockResetEnable dom, Format Word8 Word8 fmt)
    => Proxy fmt
    -> Circuit (Df dom Word8) (Df dom Word8)
format fmt = Df.compander countMin \s x ->
    let output :~> s' = case format1 fmt s of
            Punctuate (y :~> s') -> (Just y :~> s')
            Consume f -> f x
        s'' = fromMaybe countMin s'
        consume = case format1 @_ @Word8 @Word8 fmt s'' of
            Consume{} -> True
            _ -> False
    in (s'', output, consume)

formatModel :: forall fmt a b. (Format a b fmt) => Proxy fmt -> [a] -> [b]
formatModel fmt = go countMin
  where
    go s xs = case format1 fmt s of
        Punctuate (sep :~> s') -> sep : go (fromMaybe countMin s') xs
        Consume step -> case xs of
            [] -> []
            x:xs' -> let y :~> s' = step x in maybe id (:) y $ go (fromMaybe countMin s') xs'

type Fmt = Forward :++ " " :++ (Drop :++ "!" :* 3)

prop_format :: (Format Word8 Word8 fmt) => Proxy fmt -> H.Property
prop_format fmt =
    H.idWithModelSingleDomain
      H.defExpectOptions
      gen_input
      (\_ _ _ -> formatModel fmt)
      (exposeClockResetEnable @System $ format fmt)
  where
    gen_input :: H.Gen [Word8]
    gen_input = Gen.list (Range.linear 0 100) (ascii <$> Gen.alpha)
