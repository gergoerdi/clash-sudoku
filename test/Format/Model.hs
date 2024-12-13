{-# LANGUAGE RequiredTypeArguments #-}
module Format.Model where

import Clash.Prelude

import Format
import Format.Internal

import Data.Word
import Data.Maybe

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

formatModel :: forall fmt -> (Format fmt) => [Word8] -> [Word8]
formatModel fmt = go begin
  where
    begin = start fmt

    go s cs = case format1 fmt s of
        Static step -> transition step
        Dynamic step
            | (c:_) <- cs -> transition (step c)
            | otherwise -> []
      where
        next = fromMaybe begin
        output = maybe id (:)
        transition (Step consume mb_y s') = output mb_y $ go (next s') cs'
          where
            cs' | consume, (_:cs') <- cs = cs'
                | otherwise = cs

prop_format :: forall fmt -> (Format fmt) => H.Property
prop_format fmt =
    H.idWithModelSingleDomain
      H.defExpectOptions
      gen_input
      (\_ _ _ -> formatModel fmt)
      (exposeClockResetEnable @System $ format fmt)
  where
    gen_input :: H.Gen [Word8]
    gen_input = Gen.list (Range.linear 0 100) (ascii <$> Gen.alpha)
