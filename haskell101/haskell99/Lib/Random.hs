module Lib.Random (rndPerm') where

import Lib.Fn
import Lib.ListOp
import Lib.Range
import System.Random

-- Public

rndPerm' :: StdGen -> Int -> [a] -> [a]
rndPerm' g n l =
  let len = length' l
   in ( front' n
          . flatten' fromMaybe'
          . fmap' (elementAt' l)
          . fmap' (permFischerYates' g n 0 len)
      )
        (intRange' (In' 0) (Ex' len))

-- Private

-- Given a generator, n, a, b, returns a permutation of the range [a, b)
-- where the first n elements have been selected via Fischer-Yates shuffling.
permFischerYates' :: StdGen -> Int -> Int -> Int -> Int -> Int
permFischerYates' g n a b
  | n <= 0 || a >= b = id'
  | otherwise =
      let (i, g') = random g :: (Int, StdGen)
       in permFischerYates' g' (n - 1) (a + 1) b . intPerm' a (1 + mod i (b - a))

-- Given Ints a and b, return an Int -> Int function which swaps a with b
-- and acts as the identity function otherwise
intPerm' :: Int -> Int -> Int -> Int
intPerm' a b x
  | x == a = b
  | x == b = a
  | otherwise = x
