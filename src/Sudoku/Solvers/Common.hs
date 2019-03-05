{-# LANGUAGE NoImplicitPrelude #-}

module Sudoku.Solvers.Common where

import Sudoku.MessageQueue
import Sudoku.Common (Puzzle, PuzzleResults)

class Solver s where
    solve :: s -> PuzzleResults