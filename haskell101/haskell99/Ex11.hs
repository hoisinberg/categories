module Ex11 (encodeModified) where
import Lib.Either

data Encoded a = Multiple Int a | Single a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified [] = []
encodeModified (x:xs) = accumulatingEncodeModified (Single x) xs 

accumulatingEncodeModified :: (Eq a) => Encoded a -> [a] -> [Encoded a]
accumulatingEncodeModified e [] = [e]
accumulatingEncodeModified (Single x) (y:ys)
  | x == y = accumulatingEncodeModified (Multiple 2 x) ys
  | otherwise = Single x : accumulatingEncodeModified (Single y) ys
accumulatingEncodeModified (Multiple n x) (y:ys)
  | x == y = accumulatingEncodeModified (Multiple (n+1) x) ys
  | otherwise = Multiple n x : accumulatingEncodeModified (Single y) ys
