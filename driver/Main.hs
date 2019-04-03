-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
-- import           Protolude

import qualified Sudoku.Puzzles                as Puzzles
import           Sudoku.Common                  ( PuzzleResults(..)
                                                , MessageStats(..)
                                                , Puzzle
                                                )
import           Sudoku.MessageQueue
import           Sudoku.Solvers
import           Text.Printf                    ( printf )
import           Text.Layout.Table
import           Control.Monad                  ( join )
import qualified Data.Set                      as Set

printResults :: [RowGroup] -> IO ()
printResults = putStrLn . tableString
      [def, def, def, def, def, def, def]
      unicodeS
      (titlesH
            [ "Solver"
            , "Puzzle"
            , "MessageQueue"
            , "Complete?"
            , "Correct?"
            , "Messages Used"
            , "Messages Remaining"
            ]
      )

type Result = (String, String, String, PuzzleResults)

formatResults :: Result -> RowGroup
formatResults (pn, sn, qn, r) = rowG
      [ pn
      , sn
      , qn
      , show (_complete r)
      , show (_correct r)
      , show $ (_used . _stats) r
      , show $ (_remaining . _stats) r
      ]

-- todo need an HList I guess
-- queues = [("list", ListMQT), ("set", SetMQT), ("dlist", DListMQT)]
-- queues = [("set", SetMQT)]


runQueues solver pname puzzle =
      let s      = (show solver, pname, "set", solve solver wrapAsSet puzzle)
          l      = (show solver, pname, "list", solve solver wrapAsList puzzle )
          d      = (show solver, pname, "dlist", solve solver wrapAsDList puzzle)
      in  [s, l, d]

runPuzzles solver = join [ runQueues solver pname puzzle | (pname, puzzle) <- Puzzles.mostPuzzles ]

main :: IO ()
main = do
      let solnsA = runPuzzles LSWSolver
      let solnsB = runPuzzles PartialApplicationLSWSolver
      printResults $ formatResults <$> (solnsA ++ solnsB)
