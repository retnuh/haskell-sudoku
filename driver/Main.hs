-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
-- import           Protolude

import qualified Sudoku.Puzzles                as Puzzles
import           Sudoku.Common                  ( PuzzleResults(..)
                                                , MessageStats(..)
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


formatResults :: (String, String, String, PuzzleResults) -> RowGroup
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
solvers =
      [
            -- ("LSWSolver", LSWSolver), 
       ("PartialApplicationLSWSolver", PartialApplicationLSWSolver)]

runQueues sname solver pname puzzle =
      let s      = (sname, pname, "set", solve solver wrapAsSet puzzle)
          l      = (sname, pname, "list", solve solver wrapAsList puzzle)
          d      = (sname, pname, "dlist", solve solver wrapAsDList puzzle)
      in  [s, l, d]

main :: IO ()
main = do
      let solns = join
                [ runQueues sname solver pname puzzle
                | (sname, solver) <- solvers
                , (pname, puzzle) <- Puzzles.mostPuzzles
                ]
      printResults $ formatResults <$> solns
