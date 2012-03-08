module Palindrome (palindromic) where

palindromic :: Integral a => a -> Bool
palindromic a = s == reverse s
  where
    s = show a
