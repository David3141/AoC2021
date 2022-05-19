module Day06
  ( part1,
    part2,
  )
where

import Helpers (count, readCommaSeparatedInts)
import Paths_advent_of_code (getDataFileName)

part1 :: IO Int
part1 = sum . (!! 80) . iterate step . initTimers <$> input

part2 :: IO Int
part2 = sum . (!! 256) . iterate step . initTimers <$> input

-- Each element represents the count of fish with a timer equal to its position
-- [4, 7, ...] would mean there are
--   - 4 fish with timer 0
--   - 7 fish with timer 1
--   - and so on
-- To pass a day, bitshift this to the left by one.
-- Reaching a timer of -1
-- - introduces a new fish with a timer of 8
--   => a moves the end (== there are now `a` fish with a timer of 8)
-- - resets that fish to 6
--   => => There are now `a + h` fish with a timer of 6
step :: [Int] -> [Int]
step [a, b, c, d, e, f, g, h, i] = [b, c, d, e, f, g, a + h, i, a]
step _ = []

initTimers :: [Int] -> [Int]
initTimers xs = [count i xs | i <- [0 .. 8]]

input :: IO [Int]
input =
  readCommaSeparatedInts <$> (readFile =<< getDataFileName "inputs/day06.txt")
