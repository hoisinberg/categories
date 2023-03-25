module Ex16(myDrop) where
import Lib.Fn
import Lib.ListOp

myDrop :: Int -> [a] -> [a]
myDrop n
  | n > 0 = fmap' snd . filter' (\(i, _) -> rem i n > 0) . index'
  | otherwise = id'
