module CriteriaLists
( CritList(..)
, grabHead
, lst
, lst'
) where

import Iterations
import Data.List

type CritList a = [[a]]

cleanse :: CritList a -> CritList a
cleanse = filter $ (>0) . length

ordered :: Integral b => (a -> b) -> CritList a -> [a]
ordered fn lst
  | empty lst = []
  | otherwise = fst tup : ordered fn (snd tup)
    where
      tup = grabHead lst $ indexOfMax fn lst

grabHead :: CritList a -> Int -> (a, CritList a)
grabHead xs i = (item, remaining)
  where
    item = head $ xs !! i
    remaining = cleanse $ mapWithIndex skipI xs
      where
        skipI (xs', n)
          | n == i    = tail xs'
          | otherwise = xs'

indexOfMax :: Integral b => (a -> b) -> CritList a -> Int
indexOfMax fn lst = snd $ foldlWithIndex finder startVal vals
  where
    startVal = (fn (head heads), 0)
    heads = map head lst
    vals  = take 100 $ map fn heads
    finder (m, i) (x, j)
      | x > m     = (x, j)
      | otherwise = (m, i)

lst' :: CritList (Int, Int)
lst' = lst 999

lst :: Int -> CritList (Int, Int)
lst = map subList . decList
  where
    subList n = map (\i-> (n, i) ) $ decList n
    decList :: Int -> [Int]
    decList 0 = []
    decList n = n : decList (n - 1)

tupMult :: (Int, Int) -> Int
tupMult tup = (fst tup) * (snd tup)
