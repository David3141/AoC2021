module Helpers
  ( mostCommon
  , readInts
  , readCommaSeparatedInts
  , toDec
  ) where

import           Data.Char                      ( digitToInt )
import           Data.List                      ( foldl'
                                                , group
                                                , maximumBy
                                                , sort
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Ord                       ( comparing )

import           Paths_advent_of_code

readInts :: FilePath -> IO [Int]
readInts filePath =
  map read . lines <$> (readFile =<< getDataFileName filePath)

-- readCommaSeparatedInts :: FilePath -> IO [Int]
-- readCommaSeparatedInts filePath =
--   map read . splitOn "," <$> (readFile =<< getDataFileName filePath)

readCommaSeparatedInts :: String -> [Int]
readCommaSeparatedInts = map read . splitOn ","


toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0


mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (comparing length) . group . sort
