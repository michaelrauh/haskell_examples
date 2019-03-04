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
    it "takes in matrices and combines them horizontally to make them one wider" $ do
      let wordMap = Map.fromList [(["a", "b"], S.singleton "e"), (["c", "d"], S.singleton "f")]
          existingMatrices = [M.fromList 2 2 ["a","b","c","d"], M.fromList 2 2 ["b", "e","d", "f"]]
          expected = [M.fromList 2 3 ["a", "b", "e", "c", "d", "f"]]
      combineHorizontal wordMap existingMatrices `shouldBe` expected
