-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main where
-- import           Protolude

import qualified Sudoku.Puzzles                as Puzzles
import           Sudoku.Common                  ( PuzzleResults(..) )
import           Sudoku.Solvers
import           Text.Printf                    ( printf )


printResults :: (String, PuzzleResults) -> IO ()
printResults (n, r) = printf "%s: %s %s %d\n"
                             (n :: String)
                             (show (complete r))
                             (show (correct r))
                             (processedMessages r)


solvers = [LSWSolver]

main :: IO ()
main = do
  let solns =
        [ (name, solve (solver puzzle))
        | solver         <- solvers
        , (name, puzzle) <- Puzzles.mostPuzzles
        ]
  mapM_ printResults solns
