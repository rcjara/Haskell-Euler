module Primes
( isPrime
, primes
, factors
) where


primes :: [Integer]
primes = 2 : filter isPrime [3, 5..]

isPrime :: Integer -> Bool
isPrime x = not $ or $ map divides shtPrimes
  where
    divides = (== 0) . mod x
    shtPrimes = takeWhile (\n-> n^2 < x) primes

factors :: Integer -> [Integer]
factors x = subFactors primes x []
  where
    subFactors :: [Integer] -> Integer -> [Integer] -> [Integer]
    subFactors _ 1 fs = fs
    subFactors ps'@(p:ps) n fs
      | p^2 > n        = n:fs
      | n `mod` p == 0 = subFactors ps' (n `div` p) (p:fs)
      | otherwise      = subFactors ps n fs
