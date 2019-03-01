-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
-- import           Protolude

import qualified Sudoku.Puzzles                as Puzzles
import           Sudoku.Common
import           Sudoku.Solvers
import           Control.Lens
import           Text.Printf                    ( printf )
import           Control.Monad.State

-- import Debug.Trace

-- primes = 2 : oddPrimes [3, 5 ..] where
--   oddPrimes (p : ps) = trace ("oddPrimes: " ++ show p) p
--     : oddPrimes [ x | x <- ps, x `mod` p /= 0 ]


isFinished :: GameState -> Bool
isFinished gs =
  nullOf (msgs . traverse) gs || nullOf (cells . traverse . _Possibilities) gs

gamestateToPuzzle :: GameState -> Puzzle
gamestateToPuzzle = undefined

runPuzzle :: State GameState Puzzle
runPuzzle = do
  finished <- gets isFinished
  if finished then gets gamestateToPuzzle else undefined
      -- pop first msg off queue, updating state

solvePuzzle :: Puzzle -> PuzzleResults
solvePuzzle p =
  let (sol, state) = runState runPuzzle (initializeGameState p)
  in  PuzzleResults { complete          = isComplete sol
                    , correct           = isCorrect sol
                    , processedMessages = state ^. msgCount
                    , solution          = sol
                    }

printResults :: String -> PuzzleResults -> IO ()
printResults n r = putStrLn $ printf "%s: %s %s %d"
                                     (n :: String)
                                     (show (complete r))
                                     (show (correct r))
                                     (processedMessages r)

main :: IO ()
main = do
  let results = solvePuzzle Puzzles.eulerExample
  printResults "eulerExample" results
  putStrLn "hello world"
