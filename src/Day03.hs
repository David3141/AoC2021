{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day03
  ( part1
  , part2
  ) where

import           Data.Bits                      ( complement )
import           Data.List                      ( group
                                                , maximumBy
                                                , sort
                                                , transpose
                                                )
import           Helpers                        ( mostCommon
                                                , toDec
                                                )

import           Paths_advent_of_code


part1 :: IO Int
part1 = do
  bits <- map mostCommon . transpose <$> input
  let invertedBits = invertBits bits

  return $ toDec bits * toDec invertedBits


part2 :: IO Int
part2 = return 0


input = lines <$> (readFile =<< getDataFileName "inputs/day03.txt")


invertBits :: String -> String
invertBits []           = ""
invertBits (bit : rest) = invertedBit : invertBits rest
  where invertedBit = if bit == '1' then '0' else '1'
