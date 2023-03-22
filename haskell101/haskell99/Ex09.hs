module Ex09 (pack) where
import Lib.ListOp ( head' )
import Lib.Maybe ( fmap', orElse' )

pack :: (Eq a) => [a] -> [[a]]
pack = accumulatingPack []

accumulatingPack :: (Eq a) => [a] -> [a] -> [[a]]
accumulatingPack [] [] = []
accumulatingPack (x:xs) [] = [x:xs]
accumulatingPack [] (y:ys) = accumulatingPack [y] ys
accumulatingPack (x:xs) (y:ys) 
  | x == y = accumulatingPack (y:x:xs) ys
  | otherwise = (x:xs) : accumulatingPack [] (y:ys)

headEq :: (Eq a) => a -> [a] -> Bool
headEq x = orElse' False . fmap' (x ==) . head'