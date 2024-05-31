{-# LANGUAGE StandaloneDeriving, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
module Sudoku.Grid where

import Clash.Prelude
import Sudoku.Matrix

import Data.Char (ord, chr)
import Control.Monad (guard)

newtype Cell n m = Cell{ cellBits :: BitVector (n * m) }
    deriving stock (Generic)
    deriving anyclass (NFDataX)
    deriving newtype (Eq, Show)
deriving anyclass instance (KnownNat n, KnownNat m) => BitPack (Cell n m)

wild :: (KnownNat n, KnownNat m) => Cell n m
wild = Cell maxBound

combine :: (KnownNat n, KnownNat m) => Cell n m -> BitVector (n * m) -> Cell n m
combine (Cell c) knowns = Cell $ c .&. complement knowns

conflicted :: (KnownNat n, KnownNat m) => Cell n m
conflicted = Cell 0

unique :: (KnownNat n, KnownNat m) => Index (n * m) -> Cell n m
unique n = Cell $ 1 `rotateR` 1 `rotateR` fromIntegral n

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

isUnique :: (KnownNat n, KnownNat m) => Cell n m -> Bool
isUnique x = popCount (cellBits x) == 1

firstBit :: (KnownNat n) => BitVector n -> BitVector n
firstBit = v2bv . snd . mapAccumL (\ b x -> let x' = if b then low else x in (b || x' == high, x')) False . bv2v
  where
    f seen_high bit = (seen_high || bit == high, if seen_high then low else bit)

splitCell :: (KnownNat n, KnownNat m) => Cell n m -> Maybe (Cell n m, Cell n m)
splitCell (Cell s) = do
    guard $ s' /= s && s' /= 0
    return (Cell s', Cell (s .&. complement s'))
  where
    s' = firstBit s


ascii :: Char -> Unsigned 8
ascii = fromIntegral . ord

showCell :: (KnownNat n, KnownNat m, (n * m) <= 9) => Cell n m -> Unsigned 8
showCell x = case getUnique x of
    _ | x == wild -> ascii '_'
    _ | x == conflicted -> ascii 'x'
    Unset -> ascii '?'
    Unique x -> ascii '0' + 1 + fromIntegral x
    Conflict -> ascii '?'

parseCell :: (KnownNat n, KnownNat m, (n * m) <= 9) => Unsigned 8 -> Maybe (Cell n m)
parseCell x
    | x `elem` [ascii '0', ascii '_', ascii '.']
    = Just wild

    | ascii '1' <= x && x <= ascii '9'
    = Just $ unique $ fromIntegral $ x - ascii '1'

    | otherwise
    = Nothing

newtype Grid n m a = Grid{ getGrid :: Matrix n m (Matrix m n a) }
    deriving stock (Generic)
    deriving anyclass (NFDataX)
    -- deriving newtype (BitPack)

instance Functor (Grid n m) where
    fmap f = Grid . fmap (fmap f) . getGrid

instance (KnownNat n, KnownNat m) => Applicative (Grid n m) where
    pure = Grid . pure . pure
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
flattenGrid = concat . rowFirst . fmap rowFirst . getGrid

unflattenGrid :: (KnownNat n, KnownNat m, BitPack a) => Vec (n * m * m * n) a -> Grid n m a
unflattenGrid = Grid . bitCoerce

type Sudoku n m = Grid n m (Cell n m)
-- newtype Sudoku n m = Sudoku{ getSudoku :: Grid n m (Cell n m) }
--     deriving stock (Generic)
--     deriving anyclass (NFDataX)
--     -- deriving newtype (BitPack)

-- instance (KnownNat n, KnownNat m, KnownNat k, (n * m) <= 9, (n * m) ~ k + 1) => Show (Sudoku n m) where
--     show =
--         unlines . toList . fmap (toList . concat . matrixRows) . concat .
--         matrixRows . getGrid .
--         fmap (chr . fromIntegral . showCell) .
--         getSudoku

showSudoku
    :: (KnownNat n, KnownNat m, (n * m) <= 9)
    => Sudoku n m
    -> String
showSudoku =
    unlines . toList . fmap (toList . concat . matrixRows) . concat .
    matrixRows . getGrid .
    fmap (chr . fromIntegral . showCell)

emptySudoku :: (KnownNat n, KnownNat m) => Sudoku n m
emptySudoku = pure conflicted

type Coord n m = (Index n, Index m, Index m, Index n)

(!!!) :: (KnownNat n) => Vec n a -> Index n -> a
(!!!) = (!!)

boardAt :: (KnownNat n, KnownNat m) => Grid n m a -> Coord n m -> a
boardAt board (i, j, k, l) = matrixRows (matrixRows (getGrid board) !!! i !!! j) !!! k !!! l

generateGrid :: (KnownNat n, KnownNat m) => (Coord n m -> a) -> Grid n m a
generateGrid f = Grid $
    generateMatrix \i j ->
    generateMatrix \k l ->
    f (i, j, k, l)

boardToRows
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Vec (n * m) (Vec (m * n) a)
boardToRows = concatMap (fmap rowFirst) . matrixRows . getGrid

boardFromRows
    :: (KnownNat n, KnownNat m)
    => Vec (n * m) (Vec (m * n) a)
    -> Grid n m a
boardFromRows = Grid . FromRows . unconcatI . fmap (FromRows . unconcatI)

rowwise
    :: (KnownNat n, KnownNat m, 1 <= (n * m), Applicative f)
    => (Vec (m * n) a -> f (Vec (m * n) b))
    -> Grid n m a
    -> f (Grid n m b)
rowwise f = fmap boardFromRows . traverse f . boardToRows

transposeMatrix
    :: (KnownNat n, KnownNat m)
    => Matrix n m a
    -> Matrix m n a
transposeMatrix = FromRows . transpose . matrixRows

transposeGrid
    :: (KnownNat n, KnownNat m)
    => Grid n m a
    -> Grid m n a
transposeGrid = boardFromRows . transpose . boardToRows

columnwise
    :: (KnownNat n, KnownNat m, 1 <= (n * m), Applicative f)
    => (Vec (n * m) a -> f (Vec (n * m) b))
    -> Grid n m a
    -> f (Grid n m b)
columnwise f = fmap transposeGrid . rowwise f . transposeGrid

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

boxwise
    :: (KnownNat n, KnownNat m, 1 <= n, 1 <= m)
    => (Applicative f)
    => (Vec (n * m) a -> f (Vec (n * m) b))
    -> Grid n m a
    -> f (Grid n m b)
boxwise f = fmap fromBoxes . traverse f . toBoxes
