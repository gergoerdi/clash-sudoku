{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
module Format
    ( ascii
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

data Format1 a b
    = Punctuate b
    | Consume (a -> Maybe b)
    deriving (Generic, NFDataX)

class (Counter (State fmt a b), NFDataX (State fmt a b)) => Format a b (fmt :: k) where
    type State fmt a b
    format1 :: proxy fmt -> State fmt a b -> Format1 a b

-- | Consume one token of input without producing any output
data Drop

instance Format a b Drop where
    type State Drop a b = Index 1

    format1 _ _ = Consume (const Nothing)

-- | Consume one token of input and forward it to the output
data Forward

instance Format a a Forward where
    type State Forward a a = Index 1

    format1 _ _ = Consume Just

-- | Repetition
data a :* (rep :: Nat)

instance (Format a b fmt, KnownNat rep, 1 <= rep) => Format a b (fmt :* rep) where
    type State (fmt :* rep) a b = (Index rep, State fmt a b)

    format1 _ (_, fmt) = format1 (Proxy @fmt) fmt

-- | Concatenation
data a :++ b

instance (Format a b fmt1, Format a b fmt2) => Format a b (fmt1 :++ fmt2) where
    type State (fmt1 :++ fmt2) a b = Either (State fmt1 a b) (State fmt2 a b)

    format1 _ = either (format1 (Proxy @fmt1)) (format1 (Proxy @fmt2))

-- | Literal
instance (IndexableSymbol symbol, KnownNat (SymbolLength symbol), 1 <= SymbolLength symbol) => Format a Word8 symbol where
    type State symbol a Word8 = Index (SymbolLength symbol)

    format1 _ i = Punctuate $ ascii $ noDeDup $ symbolAt (Proxy @(UnconsSymbol symbol)) i

{-# INLINE format #-}
format
    :: forall dom fmt a b. (HiddenClockResetEnable dom, Format Word8 Word8 fmt)
    => Proxy fmt
    -> Circuit (Df dom Word8) (Df dom Word8)
format fmt = Df.compander countMin \s x ->
    let output = case format1 fmt s of
            Punctuate y -> Just y
            Consume f -> f x
        s' = countSucc s
        consume = case format1 fmt s' :: Format1 Word8 Word8 of
            Consume{} -> True
            _ -> False
    in (s', output, consume)

formatModel :: forall fmt a b. (Format a b fmt) => Proxy fmt -> [a] -> [b]
formatModel fmt = go countMin
  where
    go s xs = case format1 fmt s of
        Punctuate sep -> sep : go s' xs
        Consume output -> case xs of
            [] -> []
            x:xs' -> maybe id (:) (output x) $ go s' xs'
      where
        s' = countSucc s

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
