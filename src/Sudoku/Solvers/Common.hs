{-# LANGUAGE NoImplicitPrelude #-}

module Sudoku.Solvers.Common where

import Sudoku.Common (Puzzle, PuzzleResults)

class Solver a where
    solve :: a -> PuzzleResults