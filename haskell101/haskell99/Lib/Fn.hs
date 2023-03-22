module Lib.Fn where

const' :: b -> a -> b
const' x _ = x

id' :: a -> a
id' a = a

andThen :: (a -> b) -> (b -> c) -> (a -> c)
(f `andThen` g) x = g (f x)

after :: (b -> c) -> (a -> b) -> (a -> c)
(f `after` g) x = f (g x)