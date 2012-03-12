module Iterations
( mapWithIndex
, foldlWithIndex
, mapByPairs
, mapByGroups
, empty
) where

mapWithIndex :: ((a, Int) -> b) -> [a] -> [b]
mapWithIndex fn xs = map fn $ zip xs [0..]

mapByPairs :: (a -> a -> b) -> [a] -> [b]
mapByPairs _ [] = []
mapByPairs _ (x:[]) = []
mapByPairs fn (x:xs@(x':_)) = fn x x' : mapByPairs fn xs

mapByGroups :: ([a] -> b) -> Int -> [a] -> [b]
mapByGroups fn n xs@(_:xs')
  | l == n    = fn group : mapByGroups fn n xs'
  | otherwise = []
    where
      group = take n xs
      l = length group

foldlWithIndex :: (b -> (a, Int) -> b) -> b -> [a] -> b
foldlWithIndex fn init xs = foldl fn init $ zip xs [0..]

empty :: [a] -> Bool
empty = (== 0) . length
