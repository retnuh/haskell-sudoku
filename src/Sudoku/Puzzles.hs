{-# LANGUAGE NoImplicitPrelude #-}

module Sudoku.Puzzles where

import           Protolude
import           Sudoku.Common

mostPuzzles :: [Puzzle]
mostPuzzles = [eulerExample, easy, mild, difficult1, difficult25, fiendish]

allPuzzles :: [Puzzle]
allPuzzles = hardest : mostPuzzles

-- foo

-- brittany-disable-next-binding
eulerExampleSoln :: Puzzle
eulerExampleSoln =
  [ 4, 8, 3, 9, 2, 1, 6, 5, 7
  , 9, 6, 7, 3, 4, 5, 8, 2, 1
  , 2, 5, 1, 8, 7, 6, 4, 9, 3
  , 5, 4, 8, 1, 3, 2, 9, 7, 6
  , 7, 2, 9, 5, 6, 4, 1, 3, 8
  , 1, 3, 6, 7, 9, 8, 2, 4, 5
  , 3, 7, 2, 6, 8, 9, 5, 1, 4
  , 8, 1, 4, 2, 5, 3, 7, 6, 9
  , 6, 9, 5, 4, 1, 7, 3, 8, 2
  ]

-- brittany-disable-next-binding
eulerExample :: Puzzle
eulerExample =
  [ 0, 0, 3, 0, 2, 0, 6, 0, 0
  , 9, 0, 0, 3, 0, 5, 0, 0, 1
  , 0, 0, 1, 8, 0, 6, 4, 0, 0
  , 0, 0, 8, 1, 0, 2, 9, 0, 0
  , 7, 0, 0, 0, 0, 0, 0, 0, 8
  , 0, 0, 6, 7, 0, 8, 2, 0, 0
  , 0, 0, 2, 6, 0, 9, 5, 0, 0
  , 8, 0, 0, 2, 0, 3, 0, 0, 9
  , 0, 0, 5, 0, 1, 0, 3, 0, 0
  ]

-- brittany-disable-next-binding
easy :: Puzzle
easy =
  [ 7, 0, 0, 0, 8, 9, 0, 0, 0
  , 4, 0, 0, 7, 0, 0, 6, 0, 8
  , 0, 0, 8, 0, 2, 0, 7, 3, 0
  , 0, 0, 1, 2, 6, 0, 0, 9, 3
  , 0, 0, 2, 1, 0, 5, 8, 0, 0
  , 6, 3, 0, 0, 9, 7, 4, 0, 0
  , 0, 5, 7, 0, 4, 0, 2, 0, 0
  , 2, 0, 9, 0, 0, 8, 0, 0, 1
  , 0, 0, 0, 6, 1, 0, 0, 0, 5
  ]

-- brittany-disable-next-binding
mild :: Puzzle
mild =
  [ 0, 7, 0, 0, 4, 0, 0, 0, 0
  , 0, 1, 9, 0, 2, 0, 7, 0, 0
  , 0, 0, 0, 0, 0, 7, 9, 0, 5
  , 0, 0, 5, 0, 1, 9, 0, 3, 4
  , 0, 0, 2, 0, 0, 0, 6, 0, 0
  , 9, 4, 0, 8, 3, 0, 5, 0, 0
  , 2, 0, 1, 3, 0, 0, 0, 0, 0
  , 0, 0, 3, 0, 6, 0, 8, 9, 0
  , 0, 0, 0, 0, 5, 0, 0, 1, 0
  ]


-- brittany-disable-next-binding
difficult1 =
  [ 6, 0, 0, 9, 5, 0, 0, 0, 1
  , 0, 2, 0, 0, 0, 0, 0, 0, 0
  , 0, 0, 0, 4, 0, 0, 8, 2, 0
  , 2, 0, 0, 0, 6, 0, 1, 0, 0
  , 9, 0, 3, 5, 0, 4, 2, 0, 7
  , 0, 0, 8, 0, 3, 0, 0, 0, 4
  , 0, 4, 5, 0, 0, 6, 0, 0, 0
  , 0, 0, 0, 0, 0, 0, 0, 7, 0
  , 3, 0, 0, 0, 2, 5, 0, 0, 8
  ]


-- brittany-disable-next-binding
difficult25 =
  [ 0, 1, 0, 0, 8, 6, 0, 0, 0
  , 0, 6, 9, 5, 1, 0, 7, 0, 0
  , 0, 2, 0, 0, 7, 0, 0, 0, 0
  , 0, 9, 4, 7, 0, 0, 0, 0, 5
  , 0, 0, 0, 0, 0, 0, 0, 0, 0
  , 8, 0, 0, 0, 0, 5, 9, 1, 0
  , 0, 0, 0, 0, 6, 0, 0, 2, 0
  , 0, 0, 3, 0, 2, 9, 4, 8, 0
  , 0, 0, 0, 3, 5, 0, 0, 9, 0
  ]

-- brittany-disable-next-binding
fiendish =
  [ 0, 0, 1, 4, 6, 8, 0, 0, 0
  , 0, 6, 0, 3, 0, 0, 0, 0, 0
  , 0, 9, 0, 0, 0, 0, 0, 0, 2
  , 3, 0, 0, 0, 0, 0, 0, 8, 0
  , 0, 0, 5, 0, 7, 0, 9, 0, 0
  , 0, 1, 0, 0, 0, 0, 0, 0, 6
  , 8, 0, 0, 0, 0, 0, 0, 9, 0
  , 0, 0, 0, 0, 0, 4, 0, 7, 0
  , 0, 0, 0, 5, 2, 3, 4, 0, 0
  ]

-- https://curiosity.com/topics/a-finnish-mathematician-claimed-that-this-is-the-most-difficult-sudoku-puzzle-in-the-world-curiosity/
-- brittany-disable-next-binding
hardest =
  [ 8, 0, 0, 0, 0, 0, 0, 0, 0
  , 0, 0, 3, 6, 0, 0, 0, 0, 0
  , 0, 7, 0, 0, 9, 0, 2, 0, 0
  , 0, 5, 0, 0, 0, 7, 0, 0, 0
  , 0, 0, 0, 0, 4, 5, 7, 0, 0
  , 0, 0, 0, 1, 0, 0, 0, 3, 0
  , 0, 0, 1, 0, 0, 0, 0, 6, 8
  , 0, 0, 8, 5, 0, 0, 0, 1, 0
  , 0, 9, 0, 0, 0, 0, 4, 0, 0
  ]
