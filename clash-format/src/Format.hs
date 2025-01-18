{-# LANGUAGE BlockArguments, TupleSections, LambdaCase #-}
{-# LANGUAGE UndecidableInstances, FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments, RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Format
    ( Format
    , format

    , compander'

    , print, drop, wait, lit
    , str
    , loop, cond
    , (*:), (++:)
    , until, while

    , ascii
    , asciiVal
    , countSuccChecked
    ) where

import Clash.Prelude hiding (Const, drop, print, until)

import Format.Internal
import Format.SymbolAt

import Clash.Class.Counter
import Protocols
import qualified Protocols.Df as Df
import Data.Word
import Data.Maybe

compander'
    :: (HiddenClockResetEnable dom, NFDataX s)
    => s
    -> (s -> Transition s i (Maybe o))
    -> Circuit (Df dom i) (Df dom o)
compander' s0 step = Df.compander (s0, True) \(s, ready) x -> case step s of
    Const (s', y, consume) -> ((s', ready && not consume), y, False)
    Varying f
        | ready, (s', y , consume) <- f x -> ((s', not consume), y, False)
        | otherwise -> ((s, True), Nothing, True)

countSuccChecked :: (Counter a) => a -> Maybe a
countSuccChecked = countSucc . Just

singleStep :: (a :- (Maybe b, Bool)) -> Format a b
singleStep f = MkFormat () \_ -> fmap (\(y, c) -> (Nothing, y, c)) f

-- | Consume one token of input and forward it to the output
print :: Format a a
print = singleStep $ Varying \x -> (Just x, True)

-- | Consume one token of input without producing any output
drop :: Format a b
drop = singleStep $ Varying \x -> (Nothing, True)

-- | Wait until new input is available, without consuming it
wait :: Format a b
wait = singleStep $ Varying \x -> (Nothing, False)

-- | Literal atom
lit :: b -> Format a b
lit x = singleStep $ Const (Just x, False)

-- | Literal string
str :: forall (sym :: Symbol) -> (SymbolVec sym, 1 <= SymbolLength sym) => Format a Word8
str sym = MkFormat (0 :: Index (SymbolLength sym)) \i ->
    Const (countSuccChecked i, Just (xs !! i), False)
  where
    xs = symbolVec sym

-- | Unconditional infinite loop
loop :: Format a b -> Format a b
loop (MkFormat s0 step) = MkFormat s0 $ mapState (Just . fromMaybe s0) . step

-- | Concatenation
infixl 6 ++:
(++:) :: Format a b -> Format a b -> Format a b
MkFormat s1 step1 ++: MkFormat s2 step2 = MkFormat (Left s1) $ either
  (mapState (Just . maybe (Right s2) Left) . step1)
  (mapState (fmap Right) . step2)

-- | Repetition
infix 7 *:
(*:) :: forall n -> (KnownNat n, 1 <= n) => Format a b -> Format a b
n *: MkFormat s0 step = MkFormat (s0, (0 :: Index n)) \(s, i) ->
    let continue s' = Just (s', i)
        repeat = (s0,) <$> countSuccChecked i
    in mapState (maybe repeat continue) $ step s

data CondState thn els = Decide | Then thn | Else els
    deriving (Generic, NFDataX)

-- | Branching
cond :: (a -> Bool) -> Format a b -> Format a b -> Format a b
cond p (MkFormat sthn thn) (MkFormat sels els) = MkFormat Decide \case
    Decide -> branch \x -> Just $ if p x then Then sthn else Else sels
    Then s -> mapState (Then <$>) $ thn s
    Else s -> mapState (Else <$>) $ els s

branch :: (i -> s) -> Transition s i (Maybe o)
branch p = Varying \x -> (p x, Nothing, False)

data UntilState a = Check | Body a
    deriving (Generic, NFDataX)

-- | Conditional looping
until, while :: (a -> Bool) -> Format a b -> Format a b
until p (MkFormat s0 step) = MkFormat Check \case
    Check -> branch \x -> if p x then Nothing else Just $ Body s0
    Body s -> mapState (Just . maybe Check Body) $ step s
while = until . (not . )

{-# INLINE format #-}
format :: (HiddenClockResetEnable dom) => Format a b -> Circuit (Df dom a) (Df dom b)
format (MkFormat s0 step) = compander' (Just s0) \case
    Nothing -> Varying \_ -> (Nothing, Nothing, True)
    Just s -> step s
