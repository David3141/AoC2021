{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day05
  ( part1,
    part2,
  )
where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Helpers (mapmap)
import Paths_advent_of_code (getDataFileName)

type Coords = (Int, Int)

type Line = (Coords, Coords)

type Grid = M.Map Coords Int

part1 :: IO Int
part1 = length . M.filter (> 1) . toGrid . filter isStraight <$> input

part2 :: IO Int
part2 = return 2

isStraight :: Line -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

toGrid :: [Line] -> Grid
toGrid = M.fromListWith (+) . concatMap expandAndInit
  where
    expandAndInit :: Line -> [(Coords, Int)]
    expandAndInit ((x1, y1), (x2, y2))
      | x1 < x2 = [((x, y1), 1) | x <- [x1 .. x2]]
      | x1 > x2 = [((x, y1), 1) | x <- [x2 .. x1]]
      | y1 < y2 = [((x1, y), 1) | y <- [y1 .. y2]]
      | y1 > y2 = [((x1, y), 1) | y <- [y2 .. y1]]

input :: IO [Line]
input =
  map toLine
    . mapmap (splitOn ",")
    . map (splitOn " -> ")
    . lines
    <$> (readFile =<< getDataFileName "inputs/day05.txt")
  where
    toLine :: [[String]] -> Line
    toLine [[x1, y1], [x2, y2]] = ((read x1, read y1), (read x2, read y2))
