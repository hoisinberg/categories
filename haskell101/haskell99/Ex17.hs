module Ex17 (split) where

import Lib.ListOp

split :: Int -> [a] -> ([a], [a])
split n l =
  ( (fmap' snd . filter' (\(i, _) -> i < n) . index') l,
    (fmap' snd . filter' (\(i, _) -> i >= n) . index') l
  )
