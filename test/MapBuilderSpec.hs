module MapBuilderSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import MapBuilder

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "map builder" $ do
    it "does nothing" $ do
      let input = True
          expected = False
      input `shouldBe` expected
