module CombineHorizontalSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import CombineHorizontal

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "combine horizontal" $ do
    it "calculates width" $ do
      let existingMatrices = [M.fromList 2 2 ["a","b","c","d"], M.fromList 2 2 ["b", "e","d", "f"]]
          expected = 2
      findWidth existingMatrices `shouldBe` expected

    it "finds possible pairs" $ do
      let firstMatrix = M.fromList 2 2 ["a","b","c","d"]
          secondMatrix = M.fromList 2 2 ["b", "e","d", "f"]
          existingMatrices = [firstMatrix, secondMatrix]
          combinationOne = (firstMatrix, firstMatrix)
          combinationTwo = (firstMatrix, secondMatrix)
          combinationThree = (secondMatrix, firstMatrix)
          combinationFour = (secondMatrix, secondMatrix)
          expected = [combinationOne, combinationTwo, combinationThree, combinationFour]
      findPossiblePairs existingMatrices `shouldBe` expected

    it "detects if corners do not match" $ do
      let firstMatrix = M.fromList 2 2 ["a", "b", "c", "d"]
          secondMatrix = M.fromList 2 2 ["b", "e", "d", "f"]
          possiblePair = (firstMatrix, secondMatrix)
      cornersDoNotMatch possiblePair `shouldBe` True

    it "detects if corners do match" $ do
      let firstMatrix = M.fromList 2 2 ["a", "b", "c", "d"]
          secondMatrix = M.fromList 2 2 ["b", "c", "d", "f"]
          possiblePair = (firstMatrix, secondMatrix)
      cornersDoNotMatch possiblePair `shouldBe` False

    it "detects if the centers overlap" $ do
      let firstMatrix = M.fromList 2 3 ["a", "b", "c", "d", "e", "f"]
          secondMatrix = M.fromList 2 3 ["b", "c", "g", "e", "f", "h"]
          possiblePair = (firstMatrix, secondMatrix)
      centersOverlap possiblePair `shouldBe` True

    it "detects if the centers do not overlap" $ do
      let firstMatrix = M.fromList 2 3 ["a", "b", "c", "d", "e", "f"]
          secondMatrix = M.fromList 2 3 ["j", "c", "g", "e", "f", "h"]
          possiblePair = (firstMatrix, secondMatrix)
      centersOverlap possiblePair `shouldBe` False
