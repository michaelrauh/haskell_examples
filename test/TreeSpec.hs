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
            expected = Box (Point "foo") "foo"
        fromString input `shouldBe` expected

    describe "upBoxDimension" $ do
      it "combines two boxes" $ do
        let first = fromString "foo"
            second = fromString "bar"
            expected = Box (Orthotope [Point "foo", Point "bar"]) "bar"
        upBoxDimension first second `shouldBe` expected
