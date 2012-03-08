module CriteriaLists
( CritList(..)
, grabHead
, lst
) where

import Iterations
import Data.List

type CritList a = [[a]]

cleanse :: CritList a -> CritList a
cleanse = filter $ (>0) . length

grabHead :: CritList a -> Int -> (a, CritList a)
grabHead xs i = (item, remaining)
  where
    item = head $ xs !! i
    remaining = cleanse $ mapWithIndex skipI xs
      where
        skipI (xs', n)
          | n == i    = tail xs'
          | otherwise = xs'

findMax :: Ord b => (a -> b) -> CritList a -> Int

lst :: CritList Int
lst = map (\n-> [1..n]) [1..8]
