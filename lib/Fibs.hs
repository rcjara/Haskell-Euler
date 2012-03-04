module Fibs ( fibs ) where

fibs = 0 : 1 : next fibs
  where
    next (a : t@(b:_) ) = (a + b) : next t
