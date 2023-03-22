module Lib.ListOp
  ( bind',
    elementAt',
    fmap',
    foldl',
    foldr',
    generate',
    head',
    length',
    repeat',
    reverse',
    sameList',
  )
where

import Lib.Fn (const')
import Lib.Maybe (Maybe' (..))

-- Public
bind' :: [a] -> (a -> [b]) -> [b]
bind' [] _ = []
bind' (x : xs) f = concat' (f x) (bind' xs f)

concat' :: [a] -> [a] -> [a]
concat' xs ys = foldr' (:) ys xs

elementAt' :: [a] -> Int -> Maybe' a
elementAt' l n =
  if n >= 0
    then checkedElementAt l n
    else Absent

fmap' :: (a -> b) -> [a] -> [b]
fmap' _ [] = []
fmap' f (x : xs) = f x : fmap' f xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ x [] = x
foldl' f x (y : ys) = foldl' f (f x y) ys

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ x [] = x
foldr' f x (y : ys) = f y (foldr' f x ys)

generate' :: a -> (a -> Bool) -> (a -> a) -> [a]
generate' x p f = if p x then x : generate' (f x) p f else []

head' :: [a] -> Maybe' a
head' [] = Absent
head' (x:xs) = Present x

length' :: [a] -> Int
length' = foldl' (+) 0 . fmap' (const' 1)

repeat' :: Int -> a -> [a]
repeat' 0 _ = []
repeat' n x = x : repeat' (n - 1) x

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

sameList' :: (Eq a) => [a] -> [a] -> Bool
sameList' [] [] = True
sameList' [] (y : ys) = False
sameList' (x : xs) (y : ys) = (x == y) && sameList' xs ys

-- Private
checkedElementAt :: [a] -> Int -> Maybe' a
checkedElementAt [] _ = Absent
checkedElementAt (x : xs) 0 = Present x
checkedElementAt (x : xs) n = checkedElementAt xs (n - 1)
