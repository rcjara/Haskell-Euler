module Problems.P002 ( solve ) where

import Fibs

solve = sum $ filter even $ takeWhile (<= 4000000) fibs
