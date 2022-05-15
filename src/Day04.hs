{-# LANGUAGE TupleSections #-}

module Day04
  ( part1,
    part2,
  )
where

import Data.Char (digitToInt)
import Data.List
  ( transpose,
  )
import Data.List.Split (splitOn)
import Data.List.Split.Internals (chunksOf)
import Helpers
  ( mapmap,
    readCommaSeparatedInts,
  )
import Paths_advent_of_code (getDataFileName)

type BingoBoard = [[(Int, Bool)]]

type Input = ([Int], [BingoBoard])

type Win = (Int, BingoBoard)

part1 :: IO Int
part1 = checksum findFirstWin <$> input

part2 :: IO Int
part2 = checksum findLastWin <$> input

checksum :: (Input -> Win) -> Input -> Int
checksum findWin input = sumOfUnmarked * winningNum
  where
    (winningNum, winningBoard) = findWin input
    sumOfUnmarked = sum [n | (n, marked) <- concat winningBoard, not marked]

isWinner :: BingoBoard -> Bool
isWinner = any (all snd) . ((++) <*> transpose)

step :: Int -> BingoBoard -> BingoBoard
step num = mapmap (\(n, m) -> (n, m || n == num))

stepAll :: Int -> [BingoBoard] -> [BingoBoard]
stepAll num = map (step num)

input :: IO Input
input = do
  l <- filter (/= []) . lines <$> (readFile =<< getDataFileName "inputs/day04.txt")
  let firstRow = readCommaSeparatedInts $ head l
  let bingoBoards = chunksOf 5 $ map (map ((,False) . read) . words) $ tail l
  return (firstRow, bingoBoards)

findFirstWin :: Input -> Win
findFirstWin ([], _) = (-1, [])
findFirstWin (num : nums, boards)
  | isAnyWinning = (num, winningBoard)
  | otherwise = findFirstWin (nums, boardsMarked)
  where
    boardsMarked = stepAll num boards
    isAnyWinning = any isWinner boardsMarked
    winningBoard = head . filter isWinner $ boardsMarked

findLastWin :: Input -> Win
findLastWin ([], _) = (-1, [])
findLastWin (num : nums, boards)
  | areAllWinning = (num, lastWinner)
  | otherwise = findLastWin (nums, boardsMarked)
  where
    boardsMarked = stepAll num boards
    areAllWinning = all isWinner boardsMarked
    lastWinner = step num (head . filter (not . isWinner) $ boards)
