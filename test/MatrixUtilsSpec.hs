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
