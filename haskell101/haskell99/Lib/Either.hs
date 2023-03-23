module Lib.Either where
import Lib.Fn
import Lib.Maybe

data (Either' a b) = Left' a | Right' b deriving (Show)

left' :: Either' a b -> Maybe' a
left' (Right' _) = Absent
left' (Left' x) = Present x

right' :: Either' a b -> Maybe' b
right' (Left' _) = Absent
right' (Right' y) = Present y

bimap' :: (a -> c) -> (b -> d) -> Either' a b -> Either' c d
bimap' f _ (Left' x)  = Left' (f x)
bimap' _ g (Right' y) = Right' (g y)

collapse' :: (a -> c) -> (b -> c) -> Either' a b -> c
collapse' f _ (Left' x) = f x
collapse' _ g (Right' y) = g y

mapl' :: (a -> c) -> Either' a b -> Either' c b
mapl' f = bimap' f id

mapr' :: (b -> d) -> Either' a b -> Either' a d
mapr' = bimap' id
