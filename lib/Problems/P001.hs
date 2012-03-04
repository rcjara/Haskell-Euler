module Problems.P001
( filtering
, listComp
, infList
) where

probRange = [1..999]

divs :: Integral a => a -> a -> Bool
divs x = (0 ==) . mod x

filtering :: Int
filtering = sum $ filter (\n -> n `divs` 3 || n `divs` 5) probRange

listComp :: Int
listComp = sum [x | x <- probRange, x `divs` 3 || x `divs` 5]

infList :: Int
infList = sum $ takeWhile (< 1000) theList
  where
    theList = filter (\n -> n `divs` 3 || n `divs` 5) [1..]
