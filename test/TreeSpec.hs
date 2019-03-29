module TreeSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Tree

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Orthotope" $ do
    describe "upDimension" $ do
      it "makes an orthotope of two points" $ do
         let first = Point "foo"
             second = Point "bar"
             expected = Orthotope [first, second]
         upDimension first second `shouldBe` expected

      it "makes an orthotope of two orthotopes" $ do
        let first = Orthotope [Point "foo", Point "bar"]
            second = Orthotope [Point "bigfoot", Point "sasquatch"]
            expected = Orthotope [first, second]
        upDimension first second `shouldBe` expected

  describe "Box" $ do
    describe "from string" $ do
      it "makes a box from a string" $ do
        let input = "foo"
            expected = Box (Point "foo") "foo" "foo" (Point "foo")
        fromString input `shouldBe` expected

    describe "upBoxDimension" $ do
      it "combines two boxes cross-dimensionally" $ do
        let firstOrtho = Orthotope [Orthotope [Point "foo", Point "bar"], Orthotope [Point "baz", Point "bang"]]
            secondOrtho = Orthotope [Orthotope [Point "sas", Point "quatch"], Orthotope [Point "is", Point "real"]]
            firstBox = Box firstOrtho "foo" "bang" $ Orthotope [Point "bar", Point "bang"]
            secondBox = Box secondOrtho "sas" "real" $ Orthotope [Point "quatch", Point "real"]
            expected = Box (Orthotope [firstOrtho, secondOrtho]) "foo" "real" $ Orthotope [Orthotope [Point "bar", Point "bang"], Orthotope [Point "quatch", Point "real"]]
        upBoxDimension firstBox secondBox `shouldBe` expected
