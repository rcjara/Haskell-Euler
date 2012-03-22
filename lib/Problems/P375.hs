module Problems.P375
( modVal
, randNums
, indexOfRand
, nMinForRange
, ranges
, naive
, accumulate
, accumulateSolve
, startTuple
) where

import Data.List
import Primes
import qualified Data.Map as Map

modVal :: Integral a => a
modVal = 50515093

randNums :: Integral a => [a]
randNums = 290797 : nextNum 290797
  where
    nextNum x = y : nextNum y
      where
        y = x^2 `mod` modVal

indexOfRand :: (Integral a) => Int -> (a, Int)
indexOfRand i = (zip randNums [0..]) !! i

factModval = factors modVal

randFactors = map factors randNums

repeatIndices = map indexFor [0..]
  where
    indexFor x = findIndex (randNums !! 0 ==) $ take modVal $ drop (x + 1) randNums


nMinForRange (i, j) = minimum $ take (j - i + 1) $ drop i randNums

ranges :: Int -> [(Int, Int)]
ranges n = concatMap makeLowerRange [1..n]
  where
    makeLowerRange j = map makePair [1..j]
      where
        makePair i = (i, j)


--This naive attempt works, but it orders of magnitude
--too slow
naive :: (Integral a) => Int -> a
naive = sum . map nMinForRange . ranges

accumulateSolve :: (Integral a) => Int -> a
accumulateSolve n = nextAccumulate startTuple n
  where
    nextAccumulate (_, val, _) 0 = val
    nextAccumulate tup n = nextAccumulate (accumulate tup) (n - 1)

startTuple :: (Integral a) => (Int, a, Map.Map a Int)
startTuple = (0, 0, Map.empty)


accumulate :: (Integral a) => (Int, a, Map.Map a Int) -> (Int, a, Map.Map a Int)
accumulate (prevIndex, val, map) = (index, newVal, newMap)
  where
    index = prevIndex + 1
    newMap = Map.insert (fst randTup) (snd randTup) map
      where randTup = indexOfRand index
    lst = Map.toList newMap
    newVal = val + totalForThisIndex
    totalForThisIndex = sum $ Prelude.map getValForRange $ makeRange index
      where
        makeRange upper = Prelude.map (\lower -> (lower, upper)) [1..upper]
        getValForRange (i, j) = extract theVal
          where
            extract (Just (val', index')) = val'
            extract Nothing = 0
            theVal = find inRange lst
            inRange (val, lstIndex) = i <= lstIndex && j >= lstIndex
