-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}

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
import Control.Monad (join)

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


formatResults :: (String, String, String, PuzzleResults) -> RowGroup
formatResults (pn, sn, qn, r) = rowG
      [ (printf "%s,%s,%s" pn sn qn)
      , show (_complete r)
      , show (_correct r)
      , show $ (_used . _stats) r
      , show $ (_remaining . _stats) r
      ]

-- todo need an HList I guess
-- queues = [("list", ListMQT), ("set", SetMQT), ("dlist", DListMQT)]
-- queues = [("set", SetMQT)]
solvers = [("LSWSolver", LSWSolver)]

runQueues sname solver pname puzzle = 
      let l = (sname, pname, "list", solve (LSWSolver ListMQT puzzle))
          s = (sname, pname, "set", solve (LSWSolver SetMQT puzzle))
          d = (sname, pname, "dlist", solve (LSWSolver DListMQT puzzle)) in
            [s, l, d]

main :: IO ()
main = do
      let solns = join 
                [ runQueues sname solver pname puzzle
                | (sname, solver) <- solvers
                , (pname, puzzle) <- Puzzles.mostPuzzles
                ]
      printResults $ formatResults <$> solns
