module Lib.ListOp
  ( append',
    bind',
    concat',
    elementAt',
    filter',
    flatten',
    fmap',
    foldl',
    foldr',
    fromMaybe',
    front',
    generate',
    head',
    index',
    insert',
    length',
    remove',
    repeat',
    reverse',
    sameList',
    slice'
  )
where

import Lib.Fn (const')
import Lib.Maybe (Maybe' (..))
import Lib.Pair

-- Public
append' :: a -> [a] -> [a]
append' x = foldr' (:) [x]

bind' :: [a] -> (a -> [b]) -> [b]
bind' [] _ = []
bind' (x : xs) f = concat' (f x) (bind' xs f)

concat' :: [a] -> [a] -> [a]
concat' xs ys = foldr' (:) ys xs

elementAt' :: [a] -> Int -> Maybe' a
elementAt' l n =
  if n >= 0
    then checkedElementAt' l n
    else Absent

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter p xs
  | otherwise = filter p xs

flatten' :: (a -> [b]) -> [a] -> [b]
flatten' f l = bind' l f

fmap' :: (a -> b) -> [a] -> [b]
fmap' _ [] = []
fmap' f (x : xs) = f x : fmap' f xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ x [] = x
foldl' f x (y : ys) = foldl' f (f x y) ys

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ x [] = x
foldr' f x (y : ys) = f y (foldr' f x ys)

fromMaybe' :: Maybe' a -> [a]
fromMaybe' Absent = []
fromMaybe' (Present x) = [x]

front' :: Int -> [a] -> [a]
front' n = fmap' snd' . filter (\(i, _) -> i < n) . index'

generate' :: a -> (a -> Bool) -> (a -> a) -> [a]
generate' x p f = if p x then x : generate' (f x) p f else []

head' :: [a] -> Maybe' a
head' [] = Absent
head' (x:xs) = Present x

index' :: [a] -> [(Int, a)]
index' = assignIndex 0 where
  assignIndex _ [] = []
  assignIndex n (x:xs) = (n, x) : assignIndex (n+1) xs

insert' :: Int -> a -> [a] -> [a]
insert' _ x [] = [x]
insert' n x (y:ys)
  | n > 0 = y : insert' (n-1) x ys
  | otherwise = x:y:ys

length' :: [a] -> Int
length' = foldl' (+) 0 . fmap' (const' 1)

remove' :: Int -> [a] -> [a]
remove' n = fmap' snd' . filter' (\(i, _) -> i /= n) . index'

repeat' :: Int -> a -> [a]
repeat' n x
  | n > 0 = x : repeat' (n-1) x
  | otherwise = []

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

sameList' :: (Eq a) => [a] -> [a] -> Bool
sameList' [] [] = True
sameList' [] (y : ys) = False
sameList' (x : xs) (y : ys) = (x == y) && sameList' xs ys

skip' :: Int -> [a] -> [a]
skip' n = fmap' snd' . filter' (\(i, _) -> i >= n) . index'

slice' :: Int -> Int -> [a] -> [a]
slice' b e = fmap' snd . filter' (\(i, _) -> i >= b && i < e) . index'

take' :: Int -> [a] -> [a]
take' n = fmap' snd' . filter' (\(i, _) -> i < n) . index'

-- Private
checkedElementAt' :: [a] -> Int -> Maybe' a
checkedElementAt' [] _ = Absent
checkedElementAt' (x : xs) 0 = Present x
checkedElementAt' (x : xs) n = checkedElementAt' xs (n - 1)
