module Arithmetik where

pow1 b e
 | e == 0 = 1
 | otherwise = b * pow1 b (e - 1)