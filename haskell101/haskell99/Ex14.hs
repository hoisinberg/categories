module Ex14(duplicateEach) where
import Lib.ListOp

duplicateEach :: [a] -> [a]
duplicateEach l = bind' l (repeat' 2)
