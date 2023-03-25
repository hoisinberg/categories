module Ex15 where
import Lib.ListOp

repeatEach :: Int -> [a] -> [a]
repeatEach n l = bind' l (repeat' n)
