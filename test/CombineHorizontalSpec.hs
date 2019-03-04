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

    it "detects if corners and centers follow the rules" $ do
      let firstMatrix = M.fromList 2 3 ["a", "b", "c", "d", "e", "f"]
          secondMatrix = M.fromList 2 3 ["b", "c", "g", "e", "f", "h"]
          possiblePair = (firstMatrix, secondMatrix)
      filterCandidates possiblePair `shouldBe` True

    it "finds left side phrases to stitch to the right side" $ do
      let inputMatrix = M.fromList 2 3 ["a", "b", "c", "d", "e", "f"]
          expected = [["a", "b"], ["d", "e"]]
      getFroms inputMatrix `shouldBe` expected

    it "gets possible right hand side words by looking up froms in the map" $ do
      let froms = [["a", "b"], ["d", "e"]]
          nextPhrases = Map.fromList [(["a", "b"], S.fromList ["c", "x"]), (["d", "e"], S.singleton "f")]
          expected = [["c", "x"], ["f"]]
      getPossibilities froms nextPhrases `shouldBe` expected

    it "gets correspondences between possibilities and the right hand side of the right column" $ do
      let possibilities = [["c", "x"], ["f"]]
          rightMatrix = M.fromList 2 3 ["b", "c", "g", "e", "f", "h"]
          expected = [("g", ["c", "x"]), ("h", ["f"])]
      getZips possibilities rightMatrix `shouldBe` expected

    it "gets a list of booleans to see if correspondences match" $ do
      let correspondences = [("g", ["c", "g"]), ("h", ["f"])]
          expected = [True, False]
      getAnswers correspondences `shouldBe` expected

    it "checks the answers" $ do
      let answers = [True, True]
          leftMatrix = M.fromList 2 3 ["b", "c", "g", "e", "f", "h"]
      checkAnswers answers leftMatrix `shouldBe` True
