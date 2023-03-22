module Ex08 where

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:zs) = if x == y then compress (y:zs) else x : compress (y:zs)
