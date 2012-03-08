module Problems.P006 where

nums = [1..100]
sq = (^2)

sqOfSums = sq $ sum nums
sumOfSqs = sum $ map sq nums

solve = sqOfSums - sumOfSqs
