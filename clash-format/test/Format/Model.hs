{-# LANGUAGE RequiredTypeArguments #-}
module Format.Model where

import Clash.Prelude hiding (Const)

import Format
import Format.Internal

import Data.Word

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

formatModel :: Format a b -> [a] -> [b]
formatModel (MkFormat s0 step) = go (Just s0)
  where
    go s xs = case s of
        Nothing -> []
        Just s -> case step s of
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

prop_format :: Format Word8 Word8 -> H.Property
prop_format fmt =
    H.idWithModelSingleDomain
      H.defExpectOptions
      gen_input
      (\_ _ _ -> formatModel fmt)
      (exposeClockResetEnable @System $ format fmt)
  where
    gen_input :: H.Gen [Word8]
    gen_input = Gen.list (Range.linear 0 100) (ascii <$> Gen.alpha)
