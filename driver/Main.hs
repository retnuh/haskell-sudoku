-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main where
-- import           Protolude

import qualified Sudoku.Puzzles                as Puzzles
import           Sudoku.Common                  ( PuzzleResults(..) )
import           Sudoku.Solvers
import           Text.Printf                    ( printf )

-- import Debug.Trace

-- primes = 2 : oddPrimes [3, 5 ..] where
--   oddPrimes (p : ps) = trace ("oddPrimes: " ++ show p) p
--     : oddPrimes [ x | x <- ps, x `mod` p /= 0 ]

printResults :: String -> PuzzleResults -> IO ()
printResults n r = putStrLn $ printf "%s: %s %s %d"
                                     (n :: String)
                                     (show (complete r))
                                     (show (correct r))
                                     (processedMessages r)



main :: IO ()
main = do
  let solver = LSWSolver Puzzles.eulerExample
  printResults "eulerExample" (solve solver)
  putStrLn "hello world"
