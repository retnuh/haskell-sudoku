-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main where
-- import           Protolude

import qualified Sudoku.Puzzles                as Puzzles
import           Sudoku.Common                  ( PuzzleResults(..)
                                                , MessageStats(..)
                                                )
import           Sudoku.Solvers
import           Text.Printf                    ( printf )
import           Text.Layout.Table

printResults :: [RowGroup] -> IO ()
printResults = putStrLn . tableString
      [def, def, def, def, def]
      unicodeS
      (titlesH
            [ "Puzzle"
            , "Complete?"
            , "Correct?"
            , "Messages Used"
            , "Messages Remaining"
            ]
      )


formatResults :: (String, PuzzleResults) -> RowGroup
formatResults (n, r) = rowG
      [ n :: String
      , show (_complete r)
      , show (_correct r)
      , show $ (_used . _stats) r
      , show $ (_remaining . _stats) r
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
