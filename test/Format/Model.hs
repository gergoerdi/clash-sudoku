{-# LANGUAGE RequiredTypeArguments #-}
module Format.Model where

import Clash.Prelude

import Format
import Format.Internal

import Data.Word

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

formatModel :: forall fmt -> (Format fmt) => [Word8] -> [Word8]
formatModel fmt = go (Just $ start fmt)
  where
    go s xs = case s of
        Nothing -> []
        Just s -> case transition fmt s of
            Static step -> proceed step
            Dynamic f
                | (x:_) <- xs -> proceed (f x)
                | otherwise -> []
      where
        output = maybe id (:)
        proceed (Step consume mb_y s') = output mb_y $ go s' xs'
          where
            xs' | consume, (_:xs') <- xs = xs'
                | otherwise = xs

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
