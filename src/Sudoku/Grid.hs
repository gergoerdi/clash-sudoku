{-# LANGUAGE StandaloneDeriving, DerivingStrategies, DerivingVia, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module Sudoku.Grid where

import Clash.Prelude
import Sudoku.Matrix
import Format (ascii)

import Data.Char (ord, chr)
import Control.Monad (guard)
import Data.Ord (Down(..))
import Data.Word (Word8)

newtype Cell n m = Cell{ cellBits :: BitVector (n * m) }
    deriving stock (Generic)
    deriving anyclass (NFDataX)
    deriving newtype (Eq, Show)
    deriving (Ord) via Down (BitVector (n * m)) -- So that the ordering makes it easy to check solutions
deriving anyclass instance (KnownNat n, KnownNat m) => BitPack (Cell n m)

wild :: (KnownNat n, KnownNat m) => Cell n m
wild = Cell maxBound

conflicted :: (KnownNat n, KnownNat m) => Cell n m
conflicted = Cell 0

unique :: (KnownNat n, KnownNat m) => Index (n * m) -> Cell n m
unique n = Cell $ 1 `rotateR` 1 `rotateR` fromIntegral n

newtype Mask n m = Mask{ maskBits :: BitVector (n * m) }

instance (KnownNat n, KnownNat m) => Semigroup (Mask n m) where
    Mask m1 <> Mask m2 = Mask (m1 .&. m2)

instance (KnownNat n, KnownNat m) => Monoid (Mask n m) where
    mempty = Mask maxBound

applyMask :: (KnownNat n, KnownNat m) => Mask n m -> Cell n m -> Cell n m
applyMask (Mask m) (Cell c) = Cell $ c .&. m

cellMask :: (KnownNat n, KnownNat m) => Cell n m -> Mask n m
cellMask = Mask . complement . cellBits

data Unique a
    = Unique a
    | Conflict
    | Unset
    deriving (Show)

getUnique :: (KnownNat n, KnownNat m) => Cell n m -> Unique (Index (n * m))
getUnique = foldl propagate Unset . zip indicesI . bv2v . cellBits
  where
    propagate :: Unique a -> (a, Bit) -> Unique a
    propagate u (x, b) | Unset <- u, b == high = Unique x
                       | b == low             = u
                       | otherwise            = Conflict

lastBit :: (KnownNat n) => BitVector n -> BitVector n
lastBit x = x `xor` (x .&. (x - 1))

splitCell :: (KnownNat n, KnownNat m) => Cell n m -> (Cell n m, Cell n m)
splitCell (Cell c) = (Cell last, Cell rest)
  where
    last = lastBit c
    rest = c .&. complement last

type Showable n m = (KnownNat n, KnownNat m, 1 <= n, 1 <= m, n * m <= (9 + 26))

showCell :: (Showable n m) => Cell n m -> Word8
showCell x = case getUnique x of
    _ | x == wild -> ascii '_'
    _ | x == conflicted -> ascii '!'
    Unset -> ascii '?'
    Unique x
      | x' < 9 -> ascii '1' + x'
      | otherwise -> ascii 'A' + 1 + x' - 10
      where
        x' = fromIntegral x
    Conflict -> ascii '!'

type Readable n m = (KnownNat n, KnownNat m, n * m <= (9 + 26))

parseCell :: (Readable n m) => Word8 -> Maybe (Cell n m)
parseCell x
    | x `elem` [ascii '0', ascii '_', ascii '.']
    = Just wild

    | ascii '1' <= x && x <= ascii '9'
    = Just $ unique $ fromIntegral $ x - ascii '1'

    | ascii 'a' <= x && x <= ascii 'z'
    = Just $ unique $ fromIntegral $ (x - ascii 'a') + 9

    | ascii 'A' <= x && x <= ascii 'Z'
    = Just $ unique $ fromIntegral $ (x - ascii 'A') + 9

    | x `elem` [ascii '!']
    = Just conflicted

    | otherwise
    = Nothing

newtype Grid n m a = Grid{ getGrid :: Matrix n m (Matrix m n a) }
    deriving stock (Generic)
    deriving anyclass (NFDataX)
    -- deriving newtype (BitPack)

instance Functor (Grid n m) where
    -- {-# INLINE fmap #-}
    fmap f = Grid . fmap (fmap f) . getGrid

instance (KnownNat n, KnownNat m) => Applicative (Grid n m) where
    -- {-# INLINE pure #-}
    pure = Grid . pure . pure

    -- {-# INLINE (<*>) #-}
    bf <*> bx = Grid $ (<*>) <$> getGrid bf <*> getGrid bx

instance (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Foldable (Grid n m) where
    foldMap f = foldMap (foldMap f) . getGrid

instance (KnownNat n, KnownNat m, 1 <= n, 1 <= m) => Traversable (Grid n m) where
    traverse f = fmap Grid . traverse (traverse f) . getGrid

instance (KnownNat n, KnownNat m) => Bundle (Grid n m a) where
    type Unbundled dom (Grid n m a) = Grid n m (Signal dom a)

    bundle = fmap Grid . bundle . fmap bundle . getGrid
    unbundle = Grid . fmap unbundle . unbundle . fmap getGrid

flattenGrid :: Grid n m a -> Vec (n * m * m * n) a
flattenGrid = concat . toRowMajorOrder . fmap toRowMajorOrder . getGrid

unflattenGrid :: (KnownNat n, KnownNat m) => Vec (n * m * m * n) a -> Grid n m a
unflattenGrid = Grid . fmap fromRowMajorOrder . fromRowMajorOrder . unconcatI

type Sudoku n m = Grid n m (Cell n m)

showSudoku
    :: (Showable n m)
    => Sudoku n m
    -> String
showSudoku =
    unlines . toList . fmap (toList . concat . matrixRows) . concat .
    matrixRows . getGrid .
    fmap (chr . fromIntegral . showCell)

emptySudoku :: (KnownNat n, KnownNat m) => Sudoku n m
emptySudoku = pure conflicted

type Coord n m = (Index n, Index m, Index m, Index n)

others :: (1 <= n) => Vec n a -> Vec n (Vec (n - 1) a)
others (Cons x Nil) = Nil :> Nil
others (Cons x xs@(Cons _ _)) = xs :> map (x :>) (others xs)

gridToRows
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Vec (n * m) (Vec (m * n) a)
gridToRows = concatMap (fmap toRowMajorOrder) . matrixRows . getGrid

gridFromRows
    :: (KnownNat n, KnownNat m)
    => Vec (n * m) (Vec (m * n) a)
    -> Grid n m a
gridFromRows = Grid . FromRows . unconcatI . fmap (FromRows . unconcatI)

rowmap
    :: (KnownNat n, KnownNat m, 1 <= (n * m))
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Vec (n * m) b
rowmap f = fmap f . gridToRows

rowwise
    :: (KnownNat n, KnownNat m, 1 <= n * m)
    => (Vec (m * n) a -> Vec (m * n) b)
    -> Grid n m a
    -> Grid n m b
rowwise f = gridFromRows . rowmap f

transposeGrid
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Grid m n a
transposeGrid = gridFromRows . transpose . gridToRows

colmap
    :: (KnownNat n, KnownNat m, 1 <= (n * m))
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Vec (n * m) b
colmap f = rowmap f . transposeGrid

columnwise
    :: (KnownNat n, KnownNat m, 1 <= (n * m))
    => (Vec (n * m) a -> Vec (n * m) b)
    -> Grid n m a
    -> Grid n m b
columnwise f = transposeGrid . gridFromRows . colmap f

toBoxes
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Matrix n m (Vec (n * m) a)
toBoxes = FromRows . fmap (fmap concat . transpose . fmap matrixRows) . matrixRows . getGrid

fromBoxes
    :: forall n m a. (KnownNat n, KnownNat m)
    => Matrix n m (Vec (n * m) a)
    -> Grid n m a
fromBoxes = Grid . FromRows . fmap (fmap FromRows . transpose . fmap unconcatI) . matrixRows

boxmap
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => (Vec (n * m) a -> b)
    -> Grid n m a
    -> Matrix n m b
boxmap f = fmap f . toBoxes

boxwise
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => (Vec (n * m) a -> Vec (n * m) b)
    -> Grid n m a
    -> Grid n m b
boxwise f = fromBoxes . boxmap f

mapAccumGrid
    :: forall n m a s b. (KnownNat n, KnownNat m)
    => (s -> a -> (s, b))
    -> s
    -> Grid n m a
    -> (s, Grid n m b)
mapAccumGrid f s0 = fmap unflattenGrid . mapAccumL f s0 . flattenGrid

mapAccumGridB
    :: forall n m a s b dom. (KnownNat n, KnownNat m, Bundle b)
    => (Signal dom s -> Signal dom a -> Signal dom (s, b))
    -> Signal dom s
    -> Grid n m (Signal dom a)
    -> (Signal dom s, Grid n m (Unbundled dom b))
mapAccumGridB f = mapAccumGrid f'
  where
    f' s x = fmap unbundle . unbundle $ f s x
