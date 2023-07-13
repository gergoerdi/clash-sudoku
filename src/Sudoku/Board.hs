{-# LANGUAGE StandaloneDeriving, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
module Sudoku.Board where

import Clash.Prelude
import Sudoku.Matrix

import Data.Char (ord, chr)
import Control.Monad (guard)

newtype Space n m = Space{ spaceBits :: BitVector (n * m) }
    deriving stock (Generic)
    deriving anyclass (NFDataX)
    deriving newtype (Eq, Show)
deriving anyclass instance (KnownNat n, KnownNat m, 1 <= (n * m)) => BitPack (Space n m)

wild :: (KnownNat n, KnownNat m) => Space n m
wild = Space maxBound

combine :: (KnownNat n, KnownNat m) => Space n m -> Vec k (Space n m) -> Space n m
combine (Space s0) xs = Space $ fold (.&.) (s0 :> map (complement . spaceBits) xs)

conflicted :: (KnownNat n, KnownNat m) => Space n m
conflicted = Space 0

unique :: (KnownNat n, KnownNat m) => Index (n * m) -> Space n m
unique n = Space $ 1 `shiftL` fromIntegral n

data Unique a
    = Unique a
    | Conflict
    | Unset
    deriving (Show)

getUnique :: (KnownNat n, KnownNat m, KnownNat k, (n * m) ~ k + 1) => Space n m -> Unique (Index (n * m))
getUnique = fold propagate . zipWith start (reverse indicesI) . bitCoerce
  where
    start i True = Unique i
    start i False = Unset

    propagate Unset    y        = y
    propagate x        Unset    = x
    propagate _        _ = Conflict

isUnique :: (KnownNat n, KnownNat m) => Space n m -> Bool
isUnique x = popCount (spaceBits x) == 1

firstBit :: (KnownNat n) => BitVector n -> BitVector n
firstBit = v2bv . snd . mapAccumL (\ b x -> let x' = if b then low else x in (b || x' == high, x')) False . bv2v
  where
    f seen_high bit = (seen_high || bit == high, if seen_high then low else bit)

splitSpace :: (KnownNat n, KnownNat m) => Space n m -> Maybe (Space n m, Space n m)
splitSpace (Space s) = do
    guard $ s' /= s && s' /= 0
    return (Space s', Space (s .&. complement s'))
  where
    s' = firstBit s


ascii :: Char -> Unsigned 8
ascii = fromIntegral . ord

showSpace :: (KnownNat n, KnownNat m, (n * m) <= 9, KnownNat k, (n * m) ~ k + 1) => Space n m -> Unsigned 8
showSpace x = case getUnique x of
    _ | x == wild -> ascii '_'
    _ | x == conflicted -> ascii 'x'
    Unset -> ascii '?'
    Unique x -> ascii '0' + 1 + fromIntegral x
    Conflict -> ascii '?'

parseSpace :: (KnownNat n, KnownNat m, (n * m) <= 9, KnownNat k, (n * m) ~ k + 1) => Unsigned 8 -> Space n m
parseSpace x
  | x == ascii '0' = wild
  | x == ascii '_' = wild
  | otherwise = unique $ fromIntegral $ x - ascii '0' - 1

newtype Board n m a = Board{ getBoard :: Matrix n m (Matrix m n a) }
    deriving stock (Generic)
    deriving anyclass (NFDataX)
    -- deriving newtype (BitPack)

instance Functor (Board n m) where
    fmap f = Board . fmap (fmap f) . getBoard

instance (KnownNat n, KnownNat m) => Applicative (Board n m) where
    pure = Board . pure . pure
    bf <*> bx = Board $ (<*>) <$> getBoard bf <*> getBoard bx

instance (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Foldable (Board n m) where
    foldMap f = foldMap (foldMap f) . getBoard

instance (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Traversable (Board n m) where
    traverse f = fmap Board . traverse (traverse f) . getBoard

instance (KnownNat n, KnownNat m) => Bundle (Board n m a) where
    type Unbundled dom (Board n m a) = Board n m (Signal dom a)

    bundle = fmap Board . bundle . fmap bundle . getBoard
    unbundle = Board . fmap unbundle . unbundle . fmap getBoard

flattenBoard :: Board n m a -> Vec (n * m * m * n) a
flattenBoard = concat . rowFirst . fmap rowFirst . getBoard

unflattenBoard :: (KnownNat n, KnownNat m, BitPack a) => Vec (n * m * m * n) a -> Board n m a
unflattenBoard = Board . bitCoerce

type Sudoku n m = Board n m (Space n m)
-- newtype Sudoku n m = Sudoku{ getSudoku :: Board n m (Space n m) }
--     deriving stock (Generic)
--     deriving anyclass (NFDataX)
--     -- deriving newtype (BitPack)

-- instance (KnownNat n, KnownNat m, KnownNat k, (n * m) <= 9, (n * m) ~ k + 1) => Show (Sudoku n m) where
--     show =
--         unlines . toList . fmap (toList . concat . matrixRows) . concat .
--         matrixRows . getBoard .
--         fmap (chr . fromIntegral . showSpace) .
--         getSudoku

showSudoku
    :: (KnownNat n, KnownNat m, KnownNat k, (n * m) <= 9, (n * m) ~ k + 1)
    => Sudoku n m
    -> String
showSudoku =
    unlines . toList . fmap (toList . concat . matrixRows) . concat .
    matrixRows . getBoard .
    fmap (chr . fromIntegral . showSpace)

emptySudoku :: (KnownNat n, KnownNat m) => Sudoku n m
emptySudoku = pure conflicted

type Coord n m = (Index n, Index m, Index m, Index n)

(!!!) :: (KnownNat n) => Vec n a -> Index n -> a
(!!!) = (!!)

boardAt :: (KnownNat n, KnownNat m) => Board n m a -> Coord n m -> a
boardAt board (i, j, k, l) = matrixRows (matrixRows (getBoard board) !!! i !!! j) !!! k !!! l

generateBoard :: (KnownNat n, KnownNat m) => (Coord n m -> a) -> Board n m a
generateBoard f = Board $
    generateMatrix \i j ->
    generateMatrix \k l ->
    f (i, j, k, l)

boardToRows
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => Board n m a
    -> Vec (n * m) (Vec (m * n) a)
boardToRows = concatMap (fmap rowFirst) . matrixRows . getBoard

boardFromRows
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => Vec (n * m) (Vec (m * n) a)
    -> Board n m a
boardFromRows = Board . FromRows . unconcatI . fmap (FromRows . unconcatI)

rowwise
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m, 1 <= (n * m), Applicative f)
    => (Vec (m * n) a -> f (Vec (m * n) b))
    -> Board n m a
    -> f (Board n m b)
rowwise f = fmap boardFromRows . traverse f . boardToRows

transposeMatrix
    :: (KnownNat n, KnownNat m)
    => Matrix n m a
    -> Matrix m n a
transposeMatrix = FromRows . transpose . matrixRows

transposeBoard
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => Board n m a
    -> Board m n a
transposeBoard = boardFromRows . transpose . boardToRows

columnwise
    :: (KnownNat n, KnownNat m, 1 <= m, 1 <= n, 1 <= (n * m), Applicative f)
    => (Vec (n * m) a -> f (Vec (n * m) b))
    -> Board n m a
    -> f (Board n m b)
columnwise f = fmap transposeBoard . rowwise f . transposeBoard

toFields
    :: (KnownNat n, KnownNat m, 1 <= n)
    => Board n m a
    -> Matrix n m (Vec (n * m) a)
toFields = FromRows . fmap (fmap concat . transpose . fmap matrixRows) . matrixRows . getBoard

fromFields
    :: forall n m a. (KnownNat n, KnownNat m)
    => Matrix n m (Vec (n * m) a)
    -> Board n m a
fromFields = Board . FromRows . fmap (fmap FromRows . transpose . fmap unconcatI) . matrixRows

fieldwise
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => (Applicative f)
    => (Vec (n * m) a -> f (Vec (n * m) b))
    -> Board n m a
    -> f (Board n m b)
fieldwise f = fmap fromFields . traverse f . toFields
