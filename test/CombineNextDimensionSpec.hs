module CombineNextDimensionSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import CombineNextDimension

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "combine into the next dimension" $ do
    it "combines answers depthly to form deeper answers" $ do
      let firstMatrix = M.fromList 2 2 ["a", "b", "c", "d"]
          secondMatrix = M.fromList 2 2 ["e", "f", "g", "h"]
          firstBox = Square firstMatrix
          secondBox = Square secondMatrix
          inputBoxes = [firstBox, secondBox]
          expectedBoxes = [Hyper firstBox secondBox]
          nextPhrases = Map.fromList [(["a"], S.singleton "e"), (["b"], S.singleton "f"), (["c"], S.singleton "g"), (["d"], S.singleton "h")]
      combineNextDimension nextPhrases inputBoxes `shouldBe` expectedBoxes
