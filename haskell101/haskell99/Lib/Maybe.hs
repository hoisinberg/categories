module Lib.Maybe where

data Maybe' a = Present a | Absent deriving (Show)

fmap' :: (a -> b) -> Maybe' a -> Maybe' b
fmap' f Absent = Absent
fmap' f (Present x) = Present (f x)

bind' :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
bind' Absent _ = Absent
bind' (Present x) f = f x

return' :: a -> Maybe' a
return' = Present

orElse' ::  a -> Maybe' a -> a
orElse' x Absent = x
orElse' _ (Present x) = x
