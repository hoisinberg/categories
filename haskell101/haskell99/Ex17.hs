module Ex17(split) where
import Lib.ListOp

split :: Int -> [a] -> ([a], [a])
split n l
  | n >= 0 = accumulatingSplit n [] l
  | otherwise = ([], l)

accumulatingSplit :: Int -> [a] -> [a] -> ([a], [a])
accumulatingSplit 0 l1 l2 = (l1, l2)
accumulatingSplit _ l [] = (l, [])
accumulatingSplit n l (x:xs) = accumulatingSplit (n-1) (append' x l) xs
