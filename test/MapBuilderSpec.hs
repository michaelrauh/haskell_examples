module MapBuilderSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import MapBuilder

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "map builder" $ do
    it "builds a map of next words" $ do
      let input = ["a", "b", "a", "c", "d"]
          expected = Word $ Map.fromList [("a", S.fromList ["b", "c"]), ("b", S.singleton "a"), ("c", S.singleton "d")]
      buildNextWordMap input `shouldBe` expected

    it "builds a map from a phrase of a given length to a set of next words" $ do
      let input = ["a", "b", "a", "c", "d"]
          expected = Phrase $ Map.fromList [("ab", S.singleton "a"), ("ba", S.singleton "c"), ("ac", S.singleton "d")]
      buildPhraseMap input 2 `shouldBe` expected

    it "allows lookup from a map" $ do
      let input = Word $ Map.fromList [("a", S.fromList ["b", "c"]), ("b", S.singleton "a"), ("c", S.singleton "d")]
          expected = ["b", "c"]
      nextWords input "a" `shouldBe` expected

    it "returns empty if nothing is found" $ do
      let input = Word $ Map.fromList [("a", S.fromList ["b", "c"]), ("b", S.singleton "a"), ("c", S.singleton "d")]
          expected = []
      nextWords input "f" `shouldBe` expected
