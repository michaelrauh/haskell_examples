module CombineVerticalSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import CombineVertical

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "combine vertical" $ do
    it "combines answers vertically to form taller answers" $ do
--        a b     c d      a b
--        c d  +  e f  ->  c d
--                         e f
      let inputMatrices = [M.fromList 2 2 ["a", "b", "c", "d"], M.fromList 2 2 ["c", "d", "e", "f"]]
          expectedMatrices = [M.fromList 3 2 ["a", "b", "c", "d", "e", "f"]]
          nextPhrases = Map.fromList [(["a", "c"], S.singleton "e"), (["b", "d"], S.singleton "f")]
      combineVertical nextPhrases inputMatrices `shouldBe` expectedMatrices
