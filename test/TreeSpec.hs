module TreeSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Tree

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Orthotope" $ do
    describe "grow dimension" $ do
      it "makes a line of two points" $ do
        let first = Point "foo"
            second = Point "bar"
            expected = Line [first, second]
        growDimension first second `shouldBe` expected
