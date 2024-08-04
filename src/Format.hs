{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies, PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Format where

import Format.SymbolAt

import Clash.Prelude
import Clash.Class.Counter
import Clash.Class.Counter.Internal
import Protocols
import Protocols.Internal (mapCircuit)
import qualified Protocols.Df as Df
import Data.Proxy

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data Formatted c
    = Literal c
    | ForwardData
    deriving (Generic, NFDataX)

class (Counter (Ptr c fmt), NFDataX (Ptr c fmt)) => Format c (fmt :: k) where
    data Ptr c fmt
    format1 :: Ptr c fmt -> Formatted c

-- | Consume one token of input and forward it to the output
data Consume

instance Format c Consume where
    data Ptr c Consume = Consume
        deriving (Show, Generic, NFDataX, Eq, Enum, Bounded, Counter)

    format1 Consume = ForwardData

-- | Repetition
data a :* (rep :: Nat)

instance (Format c a, KnownNat rep, 1 <= rep) => Format c (a :* rep) where
    newtype Ptr c (a :* rep) = Repeat (Index rep, Ptr c a)
        deriving stock (Generic)

    format1 (Repeat (_, k)) = format1 k

deriving anyclass instance (KnownNat rep, 1 <= rep, NFDataX (Ptr c a)) => NFDataX (Ptr c (a :* rep))
deriving newtype instance (KnownNat rep, 1 <= rep, Counter (Ptr c a)) => Counter (Ptr c (a :* rep))

-- | Concatenation
data a :++ b

instance (Format c a, Format c b) => Format c (a :++ b) where
    newtype Ptr c (a :++ b) = Append (Either (Ptr c a) (Ptr c b))
        deriving stock (Generic)

    format1 (Append xy) = case xy of
        Left x -> format1 x
        Right y -> format1 y

deriving anyclass instance (NFDataX (Ptr c a), NFDataX (Ptr c b)) => NFDataX (Ptr c (a :++ b))
deriving newtype instance (Counter (Ptr c a), Counter (Ptr c b)) => Counter (Ptr c (a :++ b))

-- | Literal
instance (IndexableSymbol sep, KnownNat (SymbolLength sep), 1 <= SymbolLength sep) => Format Char sep where
    newtype Ptr Char sep = SymbolPtr (Index (SymbolLength sep))
        deriving stock (Show, Generic)
        deriving newtype (NFDataX)

    format1 (SymbolPtr i) = Literal $ noDeDup symbolAt (Proxy @(UnconsSymbol sep)) i

deriving newtype instance (KnownNat (SymbolLength sep), 1 <= SymbolLength sep) => Counter (Ptr Char sep)

format :: forall dom fmt c a. _ => Proxy fmt -> Circuit (Df dom a) (Df dom (Either c a))
format fmt = Df.expander (let ptr = countMin :: Ptr c fmt in (ptr, format1 ptr)) \(ptr, out) ->
    let ptr' = countSucc ptr
        out' = format1 ptr'
    in case out of
        Literal sep -> \_ -> ((ptr', out'), Left sep, case out' of { ForwardData -> True; _ -> False })
        ForwardData -> \x -> ((ptr', out'), Right x, False)

formatModel :: forall fmt a. _ => Proxy fmt -> [a] -> [a]
formatModel fmt = go (countMin :: Ptr a fmt)
  where
    go ptr cs = case format1 ptr of
        Literal sep -> sep : go ptr' cs
        ForwardData -> case cs of
            c:cs' -> c : go ptr' cs'
            [] -> []
      where
        ptr' = countSucc ptr

prop_format :: _ => Proxy fmt -> H.Property
prop_format fmt =
    H.idWithModelSingleDomain
      H.defExpectOptions
      gen_input
      (\_ _ _ -> formatModel fmt)
      (exposeClockResetEnable $ format @System fmt |> Df.map (either id id))
  where
    gen_input :: H.Gen [Char]
    gen_input = Gen.list (Range.linear 0 100) Gen.alpha
