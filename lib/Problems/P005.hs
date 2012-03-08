module Problems.P005 where

import Primes
import Data.List (find)

toCheck :: [Integer]
toCheck = [step, (2 * step)..upper]
  where
    upper = foldl1 (*) [1..20]
    step = foldl1 (*) [17, 19, 20]

checksOut :: Integer -> Bool
checksOut x = all (divides x) [20,19..1]

solve :: Integer
solve = extract ans
  where
    extract (Just x) = x
    extract Nothing   = -1
    ans = find checksOut toCheck
