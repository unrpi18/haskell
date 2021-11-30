module Collatz where




next :: Int -> Int
next an
  | an `mod` 2 == 0 = an `div` 2
  | (otherwise) = 3 *an + 1

collatz :: Int -> [Int]
collatz a0 = iterate next a0

num :: Int -> Int
num m = length(takeWhile(/=1) (collatz m))



maxNum a b = 