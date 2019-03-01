{-# LANGUAGE NoImplicitPrelude #-}

module Sudoku.Common where

import           Protolude
import           Data.List.Split
import           Data.IntSet

type CellIndex = Int
type CellValue = Int

type Puzzle = [Int]

data PuzzleResults = PuzzleResults {
  complete :: Bool,
  correct :: Bool,
  processedMessages :: Int ,
  solution :: Puzzle
  } deriving (Show, Eq, Ord)

puzzleIndices :: [Int]
puzzleIndices = [0 .. 80]

oneToNine :: IntSet
oneToNine = fromDistinctAscList [1 .. 9]

isComplete :: Puzzle -> Bool
isComplete puzzle = length puzzle == 81 && notElem 0 puzzle

isCorrect :: Puzzle -> Bool
isCorrect p =
  all isCorrectContainer (partitionRows p)
    && all isCorrectContainer (partitionColumns p)
    && all isCorrectContainer (partitionBoxes p)

isCorrectContainer :: [Int] -> Bool
isCorrectContainer = (== oneToNine) . fromList

partitionRows :: Puzzle -> [[Int]]
partitionRows = chunksOf 9

partitionColumns :: Puzzle -> [[Int]]
partitionColumns = transpose . partitionRows

partitionBoxes :: Puzzle -> [[Int]]
partitionBoxes =
  fmap join . chunksOf 3 . join . transpose . chunksOf 3 . chunksOf 3


rowForCell :: Int -> Int
rowForCell = flip div 9

columnForCell :: Int -> Int
columnForCell = flip mod 9

-- Boxes go down rather than across
-- 0  3  6
-- 1  4  7
-- 2  5  8

boxForCell :: Int -> Int
boxForCell cell = cell `div` 27 + (cell `mod` 9 `div` 3) * 3

