-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
-- import           Protolude

import Criterion.Main
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
import qualified Data.Vector                   as V

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
          v      = (show solver, pname, "vector", solve solver wrapAsVector puzzle)
      in  [s, l, d, v]

runPuzzles solver = join [ runQueues solver pname puzzle | (pname, puzzle) <- Puzzles.mostPuzzles ]

createBench solver pname puzzle = [
      bench (pname ++ "/set")  $ whnf (_remaining . _stats . solve solver wrapAsSet) puzzle,
      bench (pname ++ "/list")  $ whnf (_remaining . _stats . solve solver wrapAsList) puzzle,
      bench (pname ++ "/dlist")  $ whnf (_remaining . _stats . solve solver wrapAsDList) puzzle,
      bench (pname ++ "/vector")  $ whnf (_remaining . _stats . solve solver wrapAsVector) puzzle
      ]

createBenchmarks solver = bgroup (show solver) $ join [ createBench solver pname puzzle | (pname, puzzle) <- Puzzles.mostPuzzles ]


main :: IO ()
main = do
      let solnsA = runPuzzles LSWSolver
      let solnsB = runPuzzles PartialApplicationLSWSolver
      let solnsC = runPuzzles SafeLSWSolver
      printResults $ formatResults <$> (solnsA ++ solnsB ++ solnsC)
      defaultMain [createBenchmarks LSWSolver, createBenchmarks PartialApplicationLSWSolver, createBenchmarks SafeLSWSolver]

      
