-- file Spec.hs
module Sudoku.CommonSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Sudoku.Common
import qualified Sudoku.Puzzles                as Puzzles


spec :: Spec
spec = do
  describe "Sudoku.Common grid predicates" $ do
    it "knows when a puzzle is complete" $ do
      isComplete Puzzles.eulerExampleSoln `shouldBe` True
      isComplete Puzzles.eulerExample `shouldBe` False

    it "knows when a puzzle is correct" $ do
      isCorrect Puzzles.eulerExampleSoln `shouldBe` True
      isCorrect (replicate 81 1) `shouldBe` False

  describe "Sudoku.Common containerForCell lookups" $ do
    it "maps the correct row index" $ do
      rowForCell 0 `shouldBe` 0
      rowForCell 8 `shouldBe` 0
      rowForCell 26 `shouldBe` 2
      rowForCell 27 `shouldBe` 3
      rowForCell 72 `shouldBe` 8
      rowForCell 80 `shouldBe` 8
    it "maps the correct column index" $ do
      columnForCell 0 `shouldBe` 0
      columnForCell 3 `shouldBe` 3
      columnForCell 8 `shouldBe` 8
      columnForCell 26 `shouldBe` 8
      columnForCell 27 `shouldBe` 0
      columnForCell 72 `shouldBe` 0
      columnForCell 80 `shouldBe` 8
    it "maps the correct box index" $ do
      boxForCell 0 `shouldBe` 0
      boxForCell 3 `shouldBe` 3
      boxForCell 6 `shouldBe` 6
      boxForCell 8 `shouldBe` 6
      boxForCell 26 `shouldBe` 6
      boxForCell 27 `shouldBe` 1
      boxForCell 72 `shouldBe` 2
      boxForCell 64 `shouldBe` 2
      boxForCell 80 `shouldBe` 8


--   describe "Prelude.head" $ do
--     it "returns the first element of a list" $ do
--       head [23 ..] `shouldBe` (23 :: Int)

--     it "returns the first element of an *arbitrary* list" $ property $ \x xs ->
--       head (x : xs) == (x :: Int)

--     it "throws an exception if used with an empty list" $ do
--       evaluate (head []) `shouldThrow` anyException
