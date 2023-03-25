module Ex11 (encodeModified) where
import Ex10
import Lib.ListOp

data Encoded a = Multiple Int a | Single a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified = fmap' makeDesc . encode

makeDesc :: (Int, a) -> Encoded a
makeDesc (1, x) = Single x
makeDesc (n, x) = Multiple n x
