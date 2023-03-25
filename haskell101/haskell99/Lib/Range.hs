module Lib.Range (RangeBound' (In', Ex'), intRange') where

import Lib.ListOp

data RangeBound' a = In' a | Ex' a

intRange' :: RangeBound' Int -> RangeBound' Int -> [Int]
intRange' b e = cIntRange' (cIntLower' b) (cIntUpper' e)

-- Canonical int range where first argument is inclusive and second argument is exclusive
cIntRange' :: Int -> Int -> [Int]
cIntRange' b e = generate' b (< e) (+ 1)

cIntLower' :: RangeBound' Int -> Int
cIntLower' (In' i) = i
cIntLower' (Ex' i) = i + 1

cIntUpper' :: RangeBound' Int -> Int
cIntUpper' (In' i) = i + 1
cIntUpper' (Ex' i) = i
