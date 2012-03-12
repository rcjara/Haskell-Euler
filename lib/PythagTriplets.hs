module PythagTriplets
( isTriplet
, isWhole
, completeTriplet
, completedTriplet
) where

isTriplet :: Integral a => a -> a -> a -> Bool
isTriplet a b c = a^2 + b^2 == c^2

isWhole x = x == fromInteger (round x)

completeTriplet :: Integral a => a -> a -> Maybe a
completeTriplet a c
  | isWhole b = Just b'
  | otherwise = Nothing
    where
      b = sqrt $ fromIntegral $ c^2 - a^2
      b' = floor b

completedTriplet :: Integral a => a -> a -> Maybe (a, a, a)
completedTriplet a c = makeTriplet $ completeTriplet a c
  where
    makeTriplet (Just b) = Just (a, b, c)
    makeTriplet Nothing  = Nothing
