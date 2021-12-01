module Day01
  ( part1
  , part2
  ) where

import           Helpers                        ( readInts )

part1 :: IO Int
part1 = countIncrements <$> readInts "inputs/day01.txt"


part2 :: IO Int
part2 = countIncrementsInWindows <$> readInts "inputs/day01.txt"


countIncrements []  = 0
countIncrements [_] = 0
countIncrements (x : y : rest) =
  (if y > x then 1 else 0) + countIncrements (y : rest)


countIncrementsInWindows []        = 0
countIncrementsInWindows [_]       = 0
countIncrementsInWindows [_, _]    = 0
countIncrementsInWindows [_, _, _] = 0
countIncrementsInWindows (a : b : c : d : rest) =
  (if sumB > sumA then 1 else 0) + countIncrementsInWindows (b : c : d : rest)
 where
  sumA = a + b + c
  sumB = b + c + d
