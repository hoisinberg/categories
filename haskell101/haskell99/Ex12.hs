module Ex12 (decodeModified) where
import Ex11
import Lib.Fn
import Lib.ListOp

decodeModified :: [Encoded a] -> [a]
decodeModified l = bind' l (expand . makePair)

makePair :: Encoded a -> (Int, a)
makePair (Single a) = (1, a)
makePair (Multiple n a) = (n, a)

expand :: (Int, a) -> [a]
expand = uncurry' repeat'

