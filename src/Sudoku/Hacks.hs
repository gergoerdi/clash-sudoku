module Sudoku.Hacks where

import Clash.Prelude hiding (mapAccumR)

-- Workaround for https://github.com/clash-lang/clash-compiler/issues/1111 :
-- a `mapAccumR` without a corresponding black box
mapAccumR :: (acc -> a -> (acc, b)) -> acc -> Vec n a -> (acc, Vec n b)
mapAccumR _ s Nil = (s, Nil)
mapAccumR f s (Cons x xs) =
    let (s', ys) = mapAccumR f s xs
        (s'', y) = f s' x
    in (s'', Cons y ys)
