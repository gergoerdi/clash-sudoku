{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving, DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Format
    ( ascii
    , Consume
    , (:*)
    , (:++)
    , format
    , formatModel
    ) where

import Format.SymbolAt

import Clash.Prelude
import Clash.Class.Counter
import Clash.Class.Counter.Internal
import Clash.Magic
import Protocols
import Protocols.Internal (mapCircuit)
import qualified Protocols.Df as Df
import Data.Proxy
import Data.Char (ord)
import Data.Word
import Text.Printf

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

data PunctuatedBy c
    = Literal c
    | ForwardData
    deriving (Generic, NFDataX, Functor)

class (Counter (Ptr fmt), NFDataX (Ptr fmt)) => Format (fmt :: k) where
    data Ptr fmt
    format1 :: Ptr fmt -> PunctuatedBy Char

-- | Consume one token of input and forward it to the output
data Consume

instance Format Consume where
    data Ptr Consume = Consume
        deriving (Show, Generic, NFDataX, Eq, Enum, Bounded, Counter)

    format1 Consume = ForwardData

-- | Repetition
data a :* (rep :: Nat)

instance (Format a, KnownNat rep, 1 <= rep) => Format (a :* rep) where
    newtype Ptr (a :* rep) = Repeat (Index rep, Ptr a)
        deriving stock (Generic)

    format1 (Repeat (_, k)) = format1 k

deriving anyclass instance (KnownNat rep, 1 <= rep, NFDataX (Ptr a)) => NFDataX (Ptr (a :* rep))
deriving newtype instance (KnownNat rep, 1 <= rep, Counter (Ptr a)) => Counter (Ptr (a :* rep))

-- | Concatenation
data a :++ b

instance (Format a, Format b) => Format (a :++ b) where
    newtype Ptr (a :++ b) = Append (Either (Ptr a) (Ptr b))
        deriving stock (Generic)

    format1 (Append xy) = case xy of
        Left x -> format1 x
        Right y -> format1 y

deriving anyclass instance (NFDataX (Ptr a), NFDataX (Ptr b)) => NFDataX (Ptr (a :++ b))
deriving newtype instance (Counter (Ptr a), Counter (Ptr b)) => Counter (Ptr (a :++ b))

-- | Literal
instance (IndexableSymbol sep, KnownNat (SymbolLength sep), 1 <= SymbolLength sep) => Format sep where
    newtype Ptr sep = SymbolPtr (Index (SymbolLength sep))
        deriving stock (Show, Generic)
        deriving newtype (NFDataX)

    format1 (SymbolPtr i) = Literal $ noDeDup $ symbolAt (Proxy @(UnconsSymbol sep)) i

deriving newtype instance (KnownNat (SymbolLength sep), 1 <= SymbolLength sep) => Counter (Ptr sep)

{-# INLINE format #-}
format :: forall dom fmt a. _ => Proxy fmt -> Circuit (Df dom a) (Df dom (Either Word8 a))
format fmt = Df.expander (countMin :: Ptr fmt) \ptr x ->
    let ptr' = countSucc ptr
        consume = case format1 ptr' of { ForwardData -> True; _ -> False }
        output = case format1 ptr of
            Literal sep -> Left (ascii sep)
            ForwardData -> Right x
    in (ptr', output, consume)

format' :: forall dom fmt c a. _ => Proxy fmt -> Circuit (Df dom Word8) (Df dom Word8)
format' fmt = format fmt |> Df.map (either id id)

formatModel :: forall fmt a. _ => Proxy fmt -> [a] -> [Either Word8 a]
formatModel fmt = go (countMin :: Ptr fmt)
  where
    go ptr cs = case format1 ptr of
        Literal sep -> Left (ascii sep) : go ptr' cs
        ForwardData -> case cs of
            c:cs' -> Right c : go ptr' cs'
            [] -> []
      where
        ptr' = countSucc ptr

prop_format :: _ => Proxy fmt -> H.Property
prop_format fmt =
    H.idWithModelSingleDomain
      H.defExpectOptions
      gen_input
      (\_ _ _ -> formatModel fmt)
      (exposeClockResetEnable $ format @System fmt)
  where
    gen_input :: H.Gen [Word8]
    gen_input = Gen.list (Range.linear 0 100) (ascii <$> Gen.alpha)
