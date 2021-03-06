
module Sudoku.Solvers.LensStateWriterSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import qualified Sudoku.Puzzles                as Puzzles
import           Sudoku.Common
import           Sudoku.Solvers
import           Sudoku.MessageQueue

spec :: Spec
spec = describe "Sudoku.Solvers.LSWSolver" $ do
    let results = fmap (solve LSWSolver wrapAsList . snd) Puzzles.mostPuzzles
    it "solves most puzzles to completion"
        $ mapM_ (flip shouldBe True . _complete) results

    it "solves most puzzles correctly"
        $ mapM_ (flip shouldBe True . _correct) results

    it "completes but does not solve the 'hardest' puzzle"
        $ (flip shouldBe False . _correct)
        $ solve LSWSolver wrapAsList Puzzles.hardest
