
module Sudoku.Solvers.SafeLensStateWriterSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import qualified Sudoku.Puzzles                as Puzzles
import           Sudoku.Common
import           Sudoku.Solvers
import           Sudoku.MessageQueue

spec :: Spec
spec = describe "Sudoku.Solvers.SafeLSWSolver" $ do
    let results =
            fmap (solve SafeLSWSolver wrapAsList . snd) Puzzles.mostPuzzles
    it "solves most puzzles to completion"
        $ mapM_ (flip shouldBe True . _complete) results

    it "solves most puzzles correctly"
        $ mapM_ (flip shouldBe True . _correct) results

    it "completes but does not solve the 'hardest' puzzle"
        $ (flip shouldBe False . _correct)
        $ solve SafeLSWSolver wrapAsList Puzzles.hardest

    it "solves euler correctly" $ (flip shouldBe True . _correct) $ solve SafeLSWSolver wrapAsList Puzzles.eulerExample
