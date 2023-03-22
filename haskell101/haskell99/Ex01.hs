module Ex01 where
import Lib.Maybe

myLast :: [a] -> Maybe' a
myLast [] = Absent
myLast [x] = Present x
myLast (x:xs) = myLast xs
