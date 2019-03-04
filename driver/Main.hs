-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main where
-- import           Protolude

import qualified Sudoku.Puzzles                as Puzzles
import           Sudoku.Common                  ( PuzzleResults(..) )
import           Sudoku.Solvers
import           Text.Printf                    ( printf )
import           Text.Layout.Table

printResults :: [RowGroup] -> IO ()
printResults = putStrLn . tableString
      [def, def, def, def]
      unicodeS
      (titlesH ["Puzzle", "Complete?", "Correct?", "Message Count"])


formatResults :: (String, PuzzleResults) -> RowGroup
formatResults (n, r) = rowG
      [ n :: String
      , show (complete r)
      , show (correct r)
      , show (processedMessages r)
      ]


solvers = [LSWSolver]

main :: IO ()
main = do
      let solns =
                [ (name, solve (solver puzzle))
                | solver         <- solvers
                , (name, puzzle) <- Puzzles.mostPuzzles
                ]
      printResults $ formatResults <$> solns
