module TreeSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Tree

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Orthotope" $ do
    it "finds the top right corner" $ do
      let input = Word "foo"
      getTopRightCorner input `shouldBe` "foo"
