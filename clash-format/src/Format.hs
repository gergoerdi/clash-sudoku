{-# LANGUAGE BlockArguments, TupleSections, LambdaCase #-}
{-# LANGUAGE RequiredTypeArguments, RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module Format
    ( module Format

    , ascii
    , asciiVal
    ) where

import Clash.Prelude hiding (Const, drop, print, until)

import Format.Compand
import Format.Balance
import Format.SymbolAt

import Clash.Class.Counter
import Protocols
import Data.Word
import Data.Maybe
import Data.Profunctor
import Data.String

-- | A @b@ value that potentially depends on an @a@ parameter
data a :- b
    = Const b
    | Varying (a -> b)
    deriving (Functor)
infixr 0 :-

instance Profunctor (:-) where
    dimap to from = \case
        Const x -> Const (from x)
        Varying f -> Varying (from . f . to)

-- | A state transition of a @compander@ with internal state @s@, input @a@ and output @b@
type Transition s i o = i :- (s, o, Bool)

mapState :: (s -> s') -> Transition s i o -> Transition s' i o
mapState f = fmap \(s, y, consume) -> (f s, y, consume)

data Format a b where
    MkFormat :: (NFDataX s, BitPack s) => s -> (s -> Transition (Maybe s) a (Maybe b)) -> Format a b

instance Functor (Format a) where
    fmap f (MkFormat s0 step) = MkFormat s0 $ fmap (\(s', o, consumed) -> (s', f <$> o, consumed)) . step

instance Profunctor Format where
    dimap f g (MkFormat s0 step) = MkFormat s0 $ dimap f (\(s', o, consumed) -> (s', g <$> o, consumed)) . step

toCompander
    :: (HiddenClockResetEnable dom, NFDataX s)
    => s
    -> (s -> Transition s i (Maybe o))
    -> Compander i o
toCompander s0 step = Compander (s0, True) \(s, ready) x -> case step s of
    Const (s', y, consume) -> ((s', ready && not consume), y, False)
    Varying f
        | ready, (s', y, consume) <- f x -> ((s', not consume), y, False)
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

-- | Literal multi-atom
lits :: forall a n b. (KnownNat n, 1 <= n) => Vec n b -> Format a b
lits xs = MkFormat (0 :: Index n) \i -> Const (countSuccChecked i, Just (xs !! i), False)

-- | Literal string
str :: forall (sym :: Symbol) -> (SymbolVec sym, 1 <= SymbolLength sym) => Format a Word8
str sym = lits $ symbolVec sym

-- | Unconditional infinite loop
loop :: Format a b -> Format a b
loop (MkFormat s0 step) = MkFormat s0 $ mapState (Just . fromMaybe s0) . step

-- | Concatenation
instance Semigroup (Format a b) where
    MkFormat s1 step1 <> MkFormat s2 step2 = MkFormat (Left s1) $ either
        (mapState (Just . maybe (Right s2) Left) . step1)
        (mapState (fmap Right) . step2)

cat :: [Format a b] -> Format a b
cat = balancedFold \(MkFormat s _) -> bitSize s
  where
    bitSize :: forall s. (BitPack s) => s -> Natural
    bitSize _ = natToNatural @(BitSize s)

instance IsString (Format a Word8) where
    fromString = cat . fmap (lit . ascii)


-- | Repetition
infix 7 *:
(*:) :: forall n -> (KnownNat n, 1 <= n) => Format a b -> Format a b
n *: MkFormat s0 step = MkFormat (s0, (0 :: Index n)) \(s, i) ->
    let continue s' = Just (s', i)
        repeat = (s0,) <$> countSuccChecked i
    in mapState (maybe repeat continue) $ step s

data CondState thn els = Decide | Then thn | Else els
    deriving (Generic, NFDataX, BitPack)

-- | Branching
cond :: (a -> Bool) -> Format a b -> Format a b -> Format a b
cond p (MkFormat sthn thn) (MkFormat sels els) = MkFormat Decide \case
    Decide -> branch \x -> Just $ if p x then Then sthn else Else sels
    Then s -> mapState (Then <$>) $ thn s
    Else s -> mapState (Else <$>) $ els s

branch :: (i -> s) -> Transition s i (Maybe o)
branch p = Varying \x -> (p x, Nothing, False)

data UntilState a = Check | Body a
    deriving (Generic, NFDataX, BitPack)

-- | Conditional looping
until, while :: (a -> Bool) -> Format a b -> Format a b
until p (MkFormat s0 step) = MkFormat Check \case
    Check -> branch \x -> if p x then Nothing else Just $ Body s0
    Body s -> mapState (Just . maybe Check Body) $ step s
while = until . (not . )

{-# INLINE format #-}
format :: (HiddenClockResetEnable dom) => Format a b -> Circuit (Df dom a) (Df dom b)
format (MkFormat s0 step) = compand $ toCompander (Just s0) \case
    Nothing -> Varying \_ -> (Nothing, Nothing, True)
    Just s -> step s
