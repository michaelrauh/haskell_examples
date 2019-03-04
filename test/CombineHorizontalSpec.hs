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
