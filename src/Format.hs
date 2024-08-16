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
    = Nondependent (Stateful s b)
    | Dependent (a -> Stateful s (Bool, Maybe b))
    deriving (Generic, NFDataX)

mapState :: (Maybe s -> Maybe s') -> Format1 s a b -> Format1 s' a b
mapState f = \case
    Nondependent (y :~> s) -> Nondependent $ y :~> f s
    Dependent step -> Dependent $ \x -> case step x of
        y :~> s -> y :~> f s

class (Counter (State fmt a b), NFDataX (State fmt a b)) => Format a b (fmt :: k) where
    type State fmt a b

    format1 :: proxy fmt -> State fmt a b -> Format1 (State fmt a b) a b

-- | Consume one token of input without producing any output
data Drop

instance Format a b Drop where
    type State Drop a b = Index 1

    format1 _ s = Dependent \x -> (True, Nothing) :~> countSuccChecked s

-- | Consume one token of input and forward it to the output
data Forward

instance Format a a Forward where
    type State Forward a a = Index 1

    format1 _ s = Dependent \x -> (True, Just x) :~> countSuccChecked s

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

    format1 _ i = Nondependent $ ascii (noDeDup $ symbolAt (Proxy @(UnconsSymbol symbol)) i) :~> countSuccChecked i

class (KnownNat (Size fmts a b)) => Formats (a :: Type) (b :: Type) (fmts :: [k]) where
    type States fmts a b :: Type
    type Size fmts a b :: Nat

    start :: proxy1 fmts -> proxy2 a -> proxy3 b -> Index (Size fmts a b) -> States fmts a b
    delegate :: proxy fmts -> States fmts a b -> Format1 (States fmts a b) a b

instance (Format a b fmt) => Formats a b '[fmt] where
    type States '[fmt] a b = State fmt a b
    type Size '[fmt] a b = 1

    start _ _ _ _ = countMin
    delegate _ s = format1 (Proxy @fmt) s

instance (Formats a b (fmt' : fmts), Format a b fmt) => Formats a b (fmt : fmt' : fmts) where
    type States (fmt : fmt' : fmts) a b = Either (State fmt a b) (States (fmt' : fmts) a b)
    type Size (fmt : fmt' : fmts) a b = 1 + Size (fmt' : fmts) a b

    start fmt a b i = case i of
        0 -> Left countMin
        i -> Right $ start (Proxy @(fmt' : fmts)) a b (fromIntegral $ i - 1)

    delegate _ = \case
        Left s -> mapState (fmap Left) $ format1 (Proxy @fmt) s
        Right s -> mapState (fmap Right) $ delegate (Proxy @(fmt':fmts)) s

type Choose a = Either (Index 1) a

-- | Choice
instance (Formats a b fmts, Counter (States fmts a b), NFDataX (States fmts a b), 1 <= Size fmts a b) => Format a b fmts where
    type State fmts a b = Choose (States fmts a b)

    format1 fmt (Left 0) = Dependent \x ->
        let i = 0 :: Index (Size fmts a b)
        in (False, Nothing) :~> Just (Right $ start fmt (Proxy @a) (Proxy @b) i)
    format1 fmt (Right s) = mapState (fmap Right) $ delegate fmt s

{-# INLINE format #-}
format
    :: forall dom fmt a b. (HiddenClockResetEnable dom, Format Word8 Word8 fmt)
    => Proxy fmt
    -> Circuit (Df dom Word8) (Df dom Word8)
format fmt = Df.compander (countMin, True) \(s, ready) x ->
    case format1 fmt s of
        Nondependent (y :~> s')
          | let s'' = fromMaybe countMin s'
          -> ((s'', ready), Just y, False)

        Dependent f
          | not ready
          -> ((s, True), Nothing, True)

          | let (consume, y) :~> s' = f x
          , let s'' = fromMaybe countMin s'
          -> ((s'', not consume), y, False)

formatModel :: forall fmt a b. (Format a b fmt) => Proxy fmt -> [a] -> [b]
formatModel fmt = go countMin
  where
    go s xs = case format1 fmt s of
        Nondependent (sep :~> s') -> sep : go (fromMaybe countMin s') xs
        Dependent step -> case xs of
            [] -> []
            x:xs' -> let (consume, y) :~> s' = step x in maybe id (:) y $ go (fromMaybe countMin s') (if consume then xs' else xs)

-- type Fmt = '[ Forward, Drop ] :++ " " :++ (Drop :++ "!" :* 3)
type Fmt = Forward :++ "!!!"

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
