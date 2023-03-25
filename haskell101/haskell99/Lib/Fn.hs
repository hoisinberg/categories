module Lib.Fn where

after :: (b -> c) -> (a -> b) -> (a -> c)
(f `after` g) x = f (g x)

andThen :: (a -> b) -> (b -> c) -> (a -> c)
(f `andThen` g) x = g (f x)
const' :: b -> a -> b
const' x _ = x

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

id' :: a -> a
id' a = a

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y
