module Iterations
( mapWithIndex
, foldlWithIndex
) where

mapWithIndex :: ((a, Int) -> b) -> [a] -> [b]
mapWithIndex _ [] = []
mapWithIndex fn xs = map fn $ zip xs [0..]

foldlWithIndex :: (b -> (a, Int) -> b) -> b -> [a] -> b
foldlWithIndex fn init xs = foldl fn init $ zip xs [0..]
