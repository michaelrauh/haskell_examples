module OrthotopeSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Orthotope

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
    describe "fmap" $ do
      it "applies a function into a point" $ do
        fmap succ (Point 1) `shouldBe` Point 2
      it "returns empty orthotope if the orthotope is empty" $ do
        fmap (+1) (Orthotope []) `shouldBe` Orthotope []
      it "applies a function to the point inside an orthotope if there is one point" $ do
        fmap succ (Orthotope [Point 1]) `shouldBe` Orthotope [Point 2]
