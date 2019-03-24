module TreeSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Tree

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Orthotope" $ do
    describe "get top right corner" $ do
      it "returns the word when a word is passed in" $ do
        let input = Point "foo"
        getTopRightCorner input `shouldBe` "foo"
