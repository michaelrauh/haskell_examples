module MatrixUtilsSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck
import Data.Matrix as M

import MatrixUtils

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "matrix utils" $ do
    it "removes left column" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = M.fromList 3 2 ["b", "c", "e", "f", "h", "i"]
      removeLeftColumn input `shouldBe` expected

    it "gets left column" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = M.fromList 3 1 ["a", "d", "g"]
      getLeftColumn input `shouldBe` expected

    it "removes right column" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = M.fromList 3 2 ["a", "b", "d", "e", "g", "h"]
      removeRightColumn input `shouldBe` expected

    it "gets right column" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = M.fromList 3 1 ["c", "f", "i"]
      getRightColumn input `shouldBe` expected

    it "gets top right" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = "c"
      getTopRight input `shouldBe` expected

    it "gets bottom left" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = "g"
      getBottomLeft input `shouldBe` expected

    it "gets left column as a list" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = ["a", "d", "g"]
      getLeftColumnList input `shouldBe` expected

    -- it "gets right column as a list" $ do
    --   let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
    --       expected = ["c", "f", "i"]
    --   getRightColumnList input `shouldBe` expected
    --
    -- it "gets every row as a list of lists" $ do
    --   let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
    --       expected = [["a", "b", "c"], ["d", "e", "f"], ["g", "h", "i"]]
    --   getRows input `shouldBe` expected
    --
    -- it "gets every column as a list of lists" $ do
    --   let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
    --       expected = [["a", "d", "g"], ["b", "e", "h"], ["c", "f", "i"]]
    --   getColumns input `shouldBe` expected

    it "removes the top row" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = M.fromList 2 3 ["d", "e", "f", "g", "h", "i"]
      removeTopRow input `shouldBe` expected

    it "removes the bottom row" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = M.fromList 2 3 ["a", "b", "c", "d", "e", "f"]
      removeBottomRow input `shouldBe` expected
    -- 
    -- it "gets the bottom row as a list" $ do
    --   let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
    --       expected = ["g", "h", "i"]
    --   getBottomRowList input `shouldBe` expected

    it "gets the bottom row" $ do
      let input = M.fromList 3 3 ["a", "b", "c", "d", "e", "f", "g", "h", "i"]
          expected = M.fromList 1 3 ["g", "h", "i"]
      getBottomRow input `shouldBe` expected

    it "detects a transpose" $ do
      let input = M.fromList 2 2 ["a", "b", "c", "d"]
          other = M.fromList 2 2 ["a", "c", "b", "d"]
      isTranspose input other `shouldBe` True

    it "detects a non transpose" $ do
      let input = M.fromList 2 2 ["a", "b", "c", "d"]
          other = M.fromList 2 2 ["a", "b", "b", "d"]
      isTranspose input other `shouldBe` False
