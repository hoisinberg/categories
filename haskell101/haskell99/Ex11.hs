module Ex11 (Encoded(Single, Multiple), encodeModified) where
import Ex10
import Lib.ListOp

data Encoded a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = fmap' makeDesc . encode

makeDesc :: (Int, a) -> Encoded a
makeDesc (1, x) = Single x
makeDesc (n, x) = Multiple n x
