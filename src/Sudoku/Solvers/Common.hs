{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, ConstrainedClassMethods #-}

module Sudoku.Solvers.Common where

import           Sudoku.MessageQueue
import           Sudoku.Common                  ( Puzzle
                                                , PuzzleResults
                                                )
import           Protolude

class Solver s where
    type Msg s
    solve :: (Ord (Msg s), MessageQueue q (Msg s)) => ([Msg s] -> q (Msg s)) -> s -> PuzzleResults
