{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Punctuate where

import Clash.Prelude
import Clash.Class.Counter
import Clash.Class.Counter.Internal
import Protocols
import Protocols.Internal (mapCircuit)
import qualified Protocols.Df as Df
import qualified Data.List as L
import Data.Monoid (Ap(..))
import Data.Functor.Compose
import Control.Monad ((>=>))
import Data.Maybe

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Proxy

class SymbolLength_' (s :: Maybe (Char, Symbol)) where
    type SymbolLength' s :: Nat

    symbolAt :: proxy s -> Index (SymbolLength' s) -> Char

instance SymbolLength_' Nothing where
    type SymbolLength' Nothing = 0

    symbolAt _ i = errorX "impossible"

instance (SymbolLength_ s, KnownChar c, KnownNat (SymbolLength s)) => SymbolLength_' (Just '(c, s)) where
    type SymbolLength' (Just '(c, s)) = 1 + SymbolLength s

    symbolAt _ i
        | i == 0
        = charVal (Proxy @c)

        | otherwise
        = symbolAt (Proxy @(UnconsSymbol s)) (fromIntegral (i - 1))

type SymbolLength s = SymbolLength' (UnconsSymbol s)
type SymbolLength_ s = SymbolLength_' (UnconsSymbol s)

newtype Punctuate (rep :: Nat) (sep :: Symbol) a
    = MkPunctuate (Index rep, Either a (Index (SymbolLength sep)))
    deriving stock (Show, Generic)
    deriving newtype (NFDataX)

deriving newtype instance (SymbolLength_ sep, KnownNat (SymbolLength sep), 1 <= SymbolLength sep, KnownNat rep, 1 <= rep, Counter a) => Counter (Punctuate rep sep a)

type PunctuateGrid n m =
    Punctuate m "\r\n" (
    Punctuate n "\r\n" (
    Punctuate n " " (
    Punctuate m " "
    (Index 1))))

punctuateGrid :: SNat n -> SNat m -> PunctuateGrid n m
punctuateGrid SNat SNat =
    MkPunctuate . (0,) . Left $
    MkPunctuate . (0,) . Left $
    MkPunctuate . (0,) . Left $
    MkPunctuate . (0,) . Left $
    0

class Punctuating c a {- | a -> c -} where
    punctuation :: a -> Maybe c

instance (Punctuating c (Index n)) where
    punctuation _ = Nothing

instance (Punctuating Char k, SymbolLength_ sep) => Punctuating Char (Punctuate rep sep k) where
    punctuation = \case
        MkPunctuate (_, Right i) -> Just $ noDeDup symbolAt (Proxy @(UnconsSymbol sep)) i
        MkPunctuate (_, Left k) -> punctuation k

punctuate :: forall dom spec c a. _ => spec -> Circuit (Df dom a) (Df dom (Either c a))
punctuate spec = Df.expander (spec, punctuation spec) \(spec, punc) ->
    let spec' = countSucc spec
        punc' = punctuation spec'
    in case punc of
        Just sep -> \_ -> ((spec', punc'), Left sep, isNothing punc')
        Nothing -> \x -> ((spec', punc'), Right x, False)

punctuateModel :: _ => spec -> [a] -> [a]
punctuateModel spec cs = case punctuation spec of
    Just sep -> sep : punctuateModel spec' cs
    Nothing -> case cs of
        c:cs' -> c : punctuateModel spec' cs'
        [] -> []
  where
    spec' = countSucc spec

genPunctuateInput :: H.Gen [Char]
genPunctuateInput = Gen.list (Range.linear 0 100) Gen.alpha

prop_punctuate :: _ => spec -> H.Property
prop_punctuate spec =
    H.idWithModelSingleDomain
      H.defExpectOptions
      genPunctuateInput
      (\_ _ _ -> punctuateModel spec)
      (exposeClockResetEnable $ punctuate @System spec |> Df.map (either id id))
