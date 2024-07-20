{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies, StandaloneDeriving #-}
module Proto where

import Clash.Prelude
import Clash.Class.Counter
import Protocols
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

data PuncState
    = Forward
    | Punctuate
    deriving (Generic, NFDataX)

punctuate' :: (HiddenClockResetEnable dom) => b -> Circuit (Df dom a) (Df dom (Either b a))
punctuate' sep = Circuit $ mealyB step Forward
  where
    step Forward (Df.Data input, ~(Ack out_ack)) = (if out_ack then Punctuate else Forward, (Ack out_ack, Df.Data (Right input)))
    step Forward (Df.NoData, out_ack) = (Forward, (Ack False, Df.NoData))
    step Punctuate (_, ~(Ack out_ack)) = (if out_ack then Forward else Punctuate, (Ack False, Df.Data (Left sep)))

punctuate :: (HiddenClockResetEnable dom) => b -> Circuit (Df dom a) (Df dom (Either b a))
punctuate sep = Df.expander Forward \ case
    Punctuate -> \_ -> (Forward, Left sep, True)
    Forward -> \x -> (Punctuate, Right x, False)

genPunctuateInput :: H.Gen [Int]
genPunctuateInput = Gen.list (Range.linear 0 100) (genInt 10 20)
  where
    genInt a b = Gen.integral (Range.linear a b)

punctuateModel :: b -> [a] -> [Either b a]
punctuateModel sep = go
  where
    go = \case
        [] -> []
        (x:xs) -> Right x : Left sep : go xs

-- data Punctuate (n :: Nat) (sep :: Char) a
--     = Forwarding (Index n)
--     | Punctuating
--     | Done a

-- type PrettyGrid =
--     Punctuate 1 ' ' (
--     Punctuate 1 ' ' (
--     Punctuate 0 ' ' (
--     Punctuate 1 ' ' (
--     Punctuate 1 '\r' (
--     Punctuate 0 '\n' (

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

-- data Punctuate (rep :: Nat) (sep :: Symbol) a
--     = Position (Index rep) (Either (Index (SymbolLength sep)) a)
--     deriving (Show, Eq)

-- type PrettyGrid =
--     Punctuate 2 "\r\n" (
--     Punctuate 2 "\r\n" (
--     Punctuate 2 " " (
--     Punctuate 2 " "
--     ())))

-- startPrettyGrid :: PrettyGrid
-- startPrettyGrid =
--   Position 0 . Left $
--   Position 0 . Left $
--   Position 0 . Left $
--   Position 0 . Left $
--   ()


-- punctuation :: forall rep sep a. (SymbolLength_ sep) => Punctuate rep sep a -> Either Char a
-- punctuation (Position _ (Left i)) = Left $ symbolAt (Proxy @(UnconsSymbol sep)) i
-- punctuation (Position _ (Right x)) = Right x



newtype Punctuate (rep :: Nat) (sep :: Symbol) a
    = MkPunctuate (Index rep, Either a (Index (SymbolLength sep)))
    deriving (Show)

deriving newtype instance (SymbolLength_ sep, KnownNat (SymbolLength sep), 1 <= SymbolLength sep, KnownNat rep, 1 <= rep, Counter a) => Counter (Punctuate rep sep a)

-- instance Counter ()

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

punctuation :: forall rep sep a. (SymbolLength_ sep) => Punctuate rep sep a -> Either Char a
punctuation (MkPunctuate (_, Right i)) = Left $ symbolAt (Proxy @(UnconsSymbol sep)) i
punctuation (MkPunctuate (_, Left x)) = Right x

foo :: [Char] -> [Char]
foo = go startPrettyGrid
  where
    go :: PrettyGrid -> [Char] -> [Char]
    go spec cs = case (punctuation >=> punctuation >=> punctuation >=> punctuation) spec of
        Left sep -> sep : go spec' cs
        Right _ -> case cs of
            c:cs' -> c : go spec' cs'
            [] -> []
      where
        spec' = countSucc spec

type T = Punctuate 2 "2" (Punctuate 2 "1" (Index 1))

foo2 :: [Char] -> [(T, Char)]
foo2 = go (MkPunctuate . (0,) . Left $ MkPunctuate . (0,) . Left $ 0)
  where
    go :: T -> [Char] -> [(T, Char)]
    go spec@spec0 cs = case (punctuation >=> punctuation) spec of
        Left sep -> (spec0, sep) : go spec' cs
        Right _ -> case cs of
                c:cs' -> (spec0, c) : go spec' cs'
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
blah = ['a'..'z']

prop_punctuate :: (NFData b, Show b, Eq b, ShowX b, NFDataX b) => b -> H.Property
prop_punctuate sep =
  H.idWithModelSingleDomain
    H.defExpectOptions
    genPunctuateInput
    (\_ _ _ -> punctuateModel sep)
    (exposeClockResetEnable $ punctuate @System sep)
