module TreeSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Tree
import Data.Sequence

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Orthotope" $ do
    describe "get top right corner" $ do
      it "returns the word when a point is passed in" $ do
        let input = Point "foo"
        getTopRightCorner input `shouldBe` "foo"
      it "returns the last word in the sequence when a line is passed in" $ do
        let input = Line $ fromList [Point "bar", Point "foo"]
        getTopRightCorner input `shouldBe` "foo"
