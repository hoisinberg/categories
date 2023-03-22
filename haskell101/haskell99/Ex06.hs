module Ex06 where
import Lib.ListOp ( reverse', sameList' )

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome l = sameList' l (reverse' l)
