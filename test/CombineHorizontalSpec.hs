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
    it "combines answers horizontally to form wider answers" $ do
      --  a b     b c      a b c
      --  d e  +  e f  ->  d e f
      let inputCorpus = ["a", "b", "c", "d", "e", "f", "a", "d", "b", "e", "c", "f"]
          inputMatrices = [M.fromList 2 2 ["a", "b", "d", "e"], M.fromList 2 2 ["b", "c", "e", "f"]]
          expectedMatrices = [M.fromList 2 3 ["a", "b", "c", "d", "e", "f"]]
          nextPhrases = Map.fromList [(["a", "b"], S.singleton "c"), (["d", "e"], S.singleton "f")]
      combineHorizontal nextPhrases inputMatrices `shouldBe` expectedMatrices
