{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day02
  ( part1
  , part2
  ) where

import           Data.List.Split                ( splitOn )

import           Paths_advent_of_code


part1 :: IO Int
part1 = do
  (x, y) <- foldl applyCommand1 (0, 0) <$> commands
  return $ x * y


part2 :: IO Int
part2 = do
  (x, y, _) <- foldl applyCommand2 (0, 0, 0) <$> commands
  return $ x * y


commands :: IO [[String]]
commands =
  map (splitOn " ")
    .   lines
    <$> (readFile =<< getDataFileName "inputs/day02.txt")


applyCommand1 :: (Int, Int) -> [String] -> (Int, Int)
applyCommand1 (x, y) [command, read -> value]
  | command == "forward" = (x + value, y)
  | command == "up"      = (x, y - value)
  | command == "down"    = (x, y + value)


applyCommand2 :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
applyCommand2 (x, y, aim) [command, read -> value]
  | command == "forward" = (x + value, y + aim * value, aim)
  | command == "up"      = (x, y, aim - value)
  | command == "down"    = (x, y, aim + value)
