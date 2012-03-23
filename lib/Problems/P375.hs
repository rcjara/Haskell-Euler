module Problems.P375
( modVal
, randNums
, indexOfRand
, nMinForRange
, ranges
, naive
, mapAccumulate
, mapSolve
, mapTuple
) where

import Data.List
import Primes
import qualified Data.Map as M

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

mapSolve :: (Integral a) => Int -> a
mapSolve n = nextTerm mapTuple n
  where
    nextTerm (_, val, _) 0 = val
    nextTerm tup n = nextTerm (mapAccumulate tup) (n - 1)

mapTuple :: (Integral a) => (Int, a, M.Map a Int)
mapTuple = (0, 0, M.empty)


mapAccumulate :: (Integral a) => (Int, a, M.Map a Int) -> (Int, a, M.Map a Int)
mapAccumulate (prevIndex, val, map) = (index, newVal, newMap)
  where
    index = prevIndex + 1
    newMap = M.insert (fst randTup) (snd randTup) map
      where randTup = indexOfRand index
    lst = M.toList newMap
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

listSolve :: Int -> Integer
listSolve n = nextTerm listTuple n
  where
    nextTerm (_, _, _, val, _, _) 0 = val
    nextTerm tup n = nextTerm (listAccumulate tup) (n - 1)

listTuple :: ([Integer], Integer, Integer, Integer, Integer, [Integer])
listTuple = ((rands), head rands, 0, 0, 0, [])
  where rands = tail randNums

listSteps = nextStep listTuple
  where
    nextStep tup = (rel tup') : nextStep tup'
      where
        tup' = listAccumulate tup
        rel (_, a, b, c, d, e) = (a, b, c, d, e)

listAccumulate :: ([Integer], Integer, Integer, Integer, Integer, [Integer]) -> ([Integer], Integer, Integer, Integer, Integer, [Integer])
listAccumulate ((randNum:randNums), oldMin, numsInMin, runningTot, toAdd, residNums) = (randNums, newMin, newNumsInMin, newTot, newToAdd, newResidNums)
  where
    newResidNums = fst theSpan
    newMin = min oldMin randNum
    newNumsInMin = numsInMin + (toInteger $ length higherNums)
    newTot = runningTot + newToAdd
    newToAdd = toAdd + randNum - decrementToAdd - decrementedMins
    decrementedMins = (oldMin - newMin) * numsInMin
    theSpan = span (<= randNum) $ insert randNum residNums
    decrementToAdd = sum $ Prelude.map (subtract randNum) higherNums
    higherNums = snd theSpan
