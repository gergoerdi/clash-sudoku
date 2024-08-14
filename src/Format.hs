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

data PunctuatedBy c
    = Literal c
    | ForwardData
    deriving (Generic, NFDataX)

class (Counter (State fmt), NFDataX (State fmt)) => Format (fmt :: k) where
    type State fmt
    format1 :: proxy fmt -> State fmt -> PunctuatedBy Char

-- | Consume one token of input and forward it to the output
data Forward

instance Format Forward where
    type State Forward = Index 1

    format1 _ _ = ForwardData

-- | Repetition
data a :* (rep :: Nat)

instance (Format a, KnownNat rep, 1 <= rep) => Format (a :* rep) where
    type State (a :* rep) = (Index rep, State a)

    format1 _ (_, fmt) = format1 (Proxy @a) fmt

-- | Concatenation
data a :++ b

instance (Format a, Format b) => Format (a :++ b) where
    type State (a :++ b) = Either (State a) (State b)

    format1 _ = either (format1 (Proxy @a)) (format1 (Proxy @b))

-- | Literal
instance (IndexableSymbol sep, KnownNat (SymbolLength sep), 1 <= SymbolLength sep) => Format sep where
    type State sep = Index (SymbolLength sep)

    format1 _ i = Literal $ noDeDup $ symbolAt (Proxy @(UnconsSymbol sep)) i

{-# INLINE format #-}
format
    :: (HiddenClockResetEnable dom, Format fmt)
    => Proxy fmt
    -> Circuit (Df dom a) (Df dom (Either Word8 a))
format fmt = Df.expander countMin \s x ->
    let output = case format1 fmt s of
            ForwardData -> Right x
            Literal sep -> Left (ascii sep)
        s' = countSucc s
        consume = case format1 fmt s' of
            ForwardData -> True
            _ -> False
    in (s', output, consume)

format'
    :: forall dom fmt c a. (HiddenClockResetEnable dom, Format fmt)
    => Proxy fmt
    -> Circuit (Df dom Word8) (Df dom Word8)
format' fmt = format fmt |> Df.map (either id id)

formatModel :: forall fmt a. (Format fmt) => Proxy fmt -> [a] -> [Either Word8 a]
formatModel fmt = go (countMin :: State fmt)
  where
    go ptr cs = case format1 fmt ptr of
        Literal sep -> Left (ascii sep) : go ptr' cs
        ForwardData -> case cs of
            c:cs' -> Right c : go ptr' cs'
            [] -> []
      where
        ptr' = countSucc ptr

prop_format :: (Format fmt) => Proxy fmt -> H.Property
prop_format fmt =
    H.idWithModelSingleDomain
      H.defExpectOptions
      gen_input
      (\_ _ _ -> formatModel fmt)
      (exposeClockResetEnable $ format @System fmt)
  where
    gen_input :: H.Gen [Word8]
    gen_input = Gen.list (Range.linear 0 100) (ascii <$> Gen.alpha)
