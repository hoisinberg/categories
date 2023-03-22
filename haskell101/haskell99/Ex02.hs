module Ex02 where
import Lib.Maybe

penultimate :: [a] -> Maybe' a
penultimate [] = Absent
penultimate [x] = Absent
penultimate [x, _] = Present x
penultimate (x:xs) = penultimate xs