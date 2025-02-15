{-# LANGUAGE BlockArguments, TupleSections, LambdaCase #-}
{-# LANGUAGE RequiredTypeArguments, RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Format.Compand where

import Clash.Prelude

import Protocols
import qualified Protocols.Df as Df

data Compander i o where
    Compander :: (NFDataX s) => s -> (s -> i -> (s, Maybe o, Bool)) -> Compander i o

compand :: (HiddenClockResetEnable dom) => Compander i o -> Circuit (Df dom i) (Df dom o)
compand (Compander s0 step) = Df.compander s0 step

simulateCompander :: Compander a b -> [a] -> [b]
simulateCompander (Compander s0 step) = go s0
  where
    go s = \case
        [] -> []
        xs@(x:xs') -> let (s', mb_y, consume) = step s x
                     in output mb_y $ go s' $ if consume then xs' else xs

output :: Maybe a -> [a] -> [a]
output = maybe id (:)
