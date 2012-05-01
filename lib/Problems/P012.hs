module Problems.P012
( answer
) where

import qualified Primes as P
import qualified GeometricNumbers as G
import Data.List(find)

answer = maybe (-1) fst $ find ((> 500) . snd) possibilities
  where
    possibilities = zip G.triangles $ map P.numDivisors G.triangles


