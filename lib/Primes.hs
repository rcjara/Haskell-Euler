module Primes
( isPrime
, primes
, factors
, divides
, numDivisors
) where

import Data.List (groupBy)

divides :: Integral a => a -> a -> Bool
divides x = (== 0) . mod x

primes :: [Integer]
primes = 2 : filter isPrime [3, 5..]

isPrime :: Integer -> Bool
isPrime x = not $ any divs shtPrimes
  where
    divs = (== 0) . mod x
    shtPrimes = takeWhile (\n-> n^2 <= x) primes

factors :: Integer -> [Integer]
factors x = subFactors primes x []
  where
    subFactors :: [Integer] -> Integer -> [Integer] -> [Integer]
    subFactors _ 1 fs = fs
    subFactors ps'@(p:ps) n fs
      | p^2 > n        = n:fs
      | n `mod` p == 0 = subFactors ps' (n `div` p) (p:fs)
      | otherwise      = subFactors ps n fs

numDivisors :: Integer -> Int
numDivisors = product . map succ . factorLengths
  where
    factorLengths = map length . groupBy (==) . factors
