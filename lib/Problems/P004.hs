module Problems.P004 ( solve ) where

import Data.List (find, concatMap, sortBy)
import Palindrome

listOfNums :: Int -> Int -> [Int]
listOfNums upper lower = sortBy (\a b-> b `compare` a) $ concatMap innerList $ lst upper lower
  where
    innerList :: Int -> [Int]
    innerList n = map (\i -> n * i) $ lst n lower
    lst :: Int -> Int -> [Int]
    lst u l = reverse [l..u]

solve :: Int
solve = extract ans
  where
    extract :: Maybe Int -> Int
    extract (Just x) = x
    extract Nothing  = -1

    ans :: Maybe Int
    ans = find palindromic $ listOfNums 999 100

