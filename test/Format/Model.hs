{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE PolyKinds #-}
module Format.Model where

import Clash.Prelude hiding (Const)

import Format
import Format.Internal

import Data.Word

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

formatModel :: forall (fmt :: k) -> Format a b fmt => [a] -> [b]
formatModel fmt = go (Just $ start fmt)
  where
    go s xs = case s of
        Nothing -> []
        Just s -> case transition fmt s of
            Const step -> proceed step
            Varying f
                | (x:_) <- xs -> proceed (f x)
                | otherwise -> []
      where
        output = maybe id (:)
        proceed (s', mb_y, consume) = output mb_y $ go s' xs'
          where
            xs' | consume, (_:xs') <- xs = xs'
                | otherwise = xs

prop_format :: forall fmt -> (Format Char Char fmt) => H.Property
prop_format fmt =
    H.idWithModelSingleDomain
      H.defExpectOptions
      gen_input
      (\_ _ _ -> formatModel @_ @Char @Char fmt)
      (exposeClockResetEnable @System $ format fmt)
  where
    gen_input = Gen.list (Range.linear 0 100) Gen.ascii
