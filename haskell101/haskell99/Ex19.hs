module Ex19(rotate) where
import Ex17
import Lib.Fn
import Lib.ListOp
import Lib.Pair

rotate :: Int -> [a] -> [a]
rotate n
  | n >= 0 = rotateLeft n
  | otherwise = rotateRight (-n)


rotateLeft :: Int -> [a] -> [a]
rotateLeft n l = let (f, s) = split n l in concat' s f

rotateRight :: Int -> [a] -> [a]
rotateRight n = uncurry' concat' . bimap' reverse' reverse' . split n . reverse'
