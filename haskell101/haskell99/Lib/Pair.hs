module Lib.Pair where
import Lib.Fn

bimap' :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
bimap' f g (x, y) = (f x, g y)

fst' :: (a, b) -> a
fst' (x, _) = x

mapf' :: (a -> c) -> (a, b) -> (c, b)
mapf' f = bimap' f id'

maps' :: (b -> d) -> (a, b) -> (a, d)
maps' = bimap' id'

snd' :: (a, b) -> b
snd' (_, y) = y

swap' :: (a, b) -> (b, a)
swap' (x, y) = (y, x)
