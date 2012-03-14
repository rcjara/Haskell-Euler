module Problems.P375
( modVal
, randNums
, factModval
, randFactors
, ranges
, minForRange
) where

import Data.List
import Primes

modVal :: Integral a => a
modVal = 50515093

randNums :: Integral a => [a]
randNums = 290797 : nextNum 290797
  where
    nextNum x = y : nextNum y
      where
        y = x^2 `mod` modVal

factModval = factors modVal

randFactors = map factors randNums

repeatIndices = map indexFor [0..]
  where
    indexFor x = findIndex (randNums !! 0 ==) $ take modVal $ drop (x + 1) randNums


minForRange (i, j) = minimum $ take (j - i + 1) $ drop i randNums

ranges :: Int -> [(Int, Int)]
ranges n = concatMap makeLowerRange [1..n]
  where
    makeLowerRange j = map makePair [1..j]
      where
        makePair i = (i, j)


--This naive attempt works, but it orders of magnitude
--too slow
mainFun :: (Integral a) => Int -> a
mainFun = sum . map minForRange . ranges
