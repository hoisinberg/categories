module Ex03 (elementAt) where
import Lib.Maybe

elementAt :: [a] -> Int -> Maybe' a
elementAt l n = elementAt' (n >= 0) l n

elementAt' :: Bool -> [a] -> Int -> Maybe' a
elementAt' False _ _ = Absent
elementAt' _ [] _ = Absent
elementAt' True (x:xs) 0 = Present x
elementAt' True (x:xs) n = elementAt' True xs (n-1)

