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
       , Signal dom Bool
       )
stack size x0 cmd = (enable (delay False en) rd, delay False underflow)
  where
    sp = register (0 :: Index n) sp'
    (sp', en, wr, underflow) = unbundle $ interpret <$> sp <*> cmd

    interpret sp cmd = case cmd of
        Nothing -> (sp, False, Nothing, False)
        Just Pop -> (satPred SatBound sp, True, Nothing, sp == 0)
        Just (Push x) -> (sp + 1, False, Just x, False)

    rd = blockRamU NoClearOnReset size (\_ -> x0) sp (packWrite <$> sp' <*> wr)
