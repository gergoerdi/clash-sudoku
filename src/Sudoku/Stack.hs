{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}
module Sudoku.Stack where

import Clash.Prelude

import RetroClash.Utils (enable, packWrite)

data StackCmd a
    = Pop
    | Push a
    deriving (Show, Functor, Generic, NFDataX)

stack
    :: forall n a. (KnownNat n, NFDataX a, 1 <= n)
    => forall dom. (HiddenClockResetEnable dom)
    => SNat n
    -> a
    -> Signal dom (Maybe (StackCmd a))
    -> ( Signal dom (Maybe a)
       , Signal dom (Index n)
       )
stack size x0 cmd = (enable (delay False en) rd, sp)
  where
    sp = register 0 sp'
    (sp', en, wr) = unbundle $ interpret <$> sp <*> cmd

    interpret sp cmd = case cmd of
        Nothing -> (sp, False, Nothing)
        Just Pop -> (satPred SatWrap sp, True, Nothing)
        Just (Push x) -> (satSucc SatWrap sp, False, Just x)

    rd = blockRamU NoClearOnReset size (\_ -> x0) sp (packWrite <$> sp' <*> wr)
