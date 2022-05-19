module Day07
  ( part1,
    part2,
  )
where

import Helpers (count, readCommaSeparatedInts)
import Paths_advent_of_code (getDataFileName)

part1 :: IO Int
part1 = findMinimumDistanceBy (\a b -> abs (a - b)) <$> input

part2 :: IO Int
part2 = findMinimumDistanceBy gaussSum <$> input

gaussSum :: Int -> Int -> Int
gaussSum n m = x * (x + 1) `div` 2
  where
    x = abs (m - n)

findMinimumDistanceBy :: (Int -> Int -> Int) -> [Int] -> Int
findMinimumDistanceBy distanceFunc xs =
  minimum . map sumDistances $ [minimum xs .. maximum xs]
  where
    sumDistances :: Int -> Int
    sumDistances val = sum . map (distanceFunc val) $ xs

input :: IO [Int]
input =
  readCommaSeparatedInts <$> (readFile =<< getDataFileName "inputs/day07.txt")
