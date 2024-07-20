{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
module Proto where

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

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Control.DeepSeq (NFData)

import Data.Proxy

import Debug.Trace

data Op = Add Int
    deriving (Generic, NFDataX)

asumF :: (HiddenClockResetEnable dom) => [Signal dom (Maybe a)] -> Signal dom (Maybe a)
asumF [] = pure Nothing
asumF (s:ss) = liftA2 (<|>) s (asumF ss)

daisyChain
    :: forall dom n n0. (HiddenClockResetEnable dom, KnownNat n, n ~ n0 + 1)
    => SNat n
    -> Signal dom (Maybe Op)
    -> Signal dom (Maybe Int)
    -> ( Signal dom Int
       , Signal dom (Vec n Int)
       )
daisyChain n op shift_in = (head xs, bundle xs)
  where
    xs = unfoldr n mk_cell shift_in

    mk_cell shift_in = (r, liftA2 (<$) r shift_in)
      where
        r = regMaybe 0 $ asumF
          [ shift_in
          , transform <$> op <*> r
          ]

    transform Nothing x = Nothing
    transform (Just (Add y)) x = Just $ x + y

test = simulateN @System 30 (bundle . uncurry (daisyChain (SNat @10)) . unbundle) $ L.zip ops $ (fmap Just [(0 :: Int)..9]) <> L.repeat Nothing
  where
    ops = L.replicate 14 (Just $ Add 1) <> L.repeat Nothing

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

type PrettyGrid =
    Punctuate 2 "\r\n" (
    Punctuate 2 "\r\n" (
    Punctuate 2 " " (
    Punctuate 2 " "
    (Index 1))))

startPrettyGrid :: PrettyGrid
startPrettyGrid =
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
        MkPunctuate (_, Right i) -> Just $ symbolAt (Proxy @(UnconsSymbol sep)) i
        MkPunctuate (_, Left k) -> punctuation k

punctuate :: forall dom spec c a. _ => spec -> Circuit (Df dom a) (Df dom (Either c a))
punctuate spec = Df.expander spec \s ->
    case punctuation s of
        Just sep -> \_ -> (s', Left sep, False)
          where
            (overflow, s') = countSuccOverflow s
        Nothing -> \x -> (countSucc s, Right x, True)

punctuateModel :: _ => spec -> [Char] -> [Char]
punctuateModel spec [] = []
punctuateModel spec cs = case punctuation spec of
    Just sep -> sep : punctuateModel spec' cs
    Nothing -> case cs of
        c:cs' -> c : punctuateModel spec' cs'
        [] -> []
  where
    spec' = countSucc spec

prettyGridModel :: [Char] -> [Char]
prettyGridModel (a0:a1:a2:a3:b0:b1:b2:b3:c0:c1:c2:c3:d0:d1:d2:d3:_) =
    [ a0, ' ', a1, ' ', ' ', a2, ' ', a3, ' ' , ' ', '\r', '\n'
    , b0, ' ', b1, ' ', ' ', b2, ' ', b3, ' ' , ' ', '\r', '\n'
    , '\r', '\n'
    , c0, ' ', c1, ' ', ' ', c2, ' ', c3, ' ', ' ', '\r', '\n'
    , d0, ' ', d1, ' ', ' ', d2, ' ', d3, ' ', ' ', '\r', '\n'
    , '\r', '\n'
    ]

blah :: [Char]
blah = ['a'..'z'] <> ['0'..'9']

genPunctuateInput :: H.Gen [Char]
genPunctuateInput = Gen.list (Range.linear 0 100) Gen.alpha

prop_punctuate :: _ => spec -> H.Property
prop_punctuate spec =
    H.idWithModelSingleDomain
      H.defExpectOptions
      genPunctuateInput
      (\_ _ _ -> punctuateModel spec)
      (exposeClockResetEnable $ punctuate @System spec |> Df.map (either id id))
