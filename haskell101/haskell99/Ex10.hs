module Ex10 (encode) where

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = accumulatingEncode (1, x) xs

accumulatingEncode :: (Eq a) => (Int, a) -> [a] -> [(Int, a)]
accumulatingEncode s [] = [s]
accumulatingEncode (n, x) (y:ys)
  | x == y = accumulatingEncode (n+1, x) ys
  | otherwise = (n, x) : accumulatingEncode (1, y) ys
