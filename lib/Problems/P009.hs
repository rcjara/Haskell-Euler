module Problems.P009 where
import Data.List (find)
import PythagTriplets (isTriplet)

possibleTriplets :: Integral a => [(a, a, a)]
possibleTriplets = concatMap subTriplets [998, 997..334]
  where
    subTriplets i = map makeTriplet [(u `div` 2)..(u - 1)]
      where
        u = 1000 - i
        makeTriplet j = (1000 - i - j, j, i)

check :: Integral a => (a, a, a) -> Bool
check (a, b, c) = isTriplet a b c

correctTriplet = extract $ find check possibleTriplets
  where
    extract (Just x) = x

solve = prod correctTriplet
  where
    prod (a, b, c) = a * b * c

