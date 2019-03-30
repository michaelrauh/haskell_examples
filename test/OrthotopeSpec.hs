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

        -- todo add test for find right column as it is currently only tested through box
        -- instead of this todo, move data declaration for box somewhere else so that type constructor will not be exposed
        -- also make fromString take a tuple of strings
        -- then make it so that upDimension does not even match on points but expects orthos
        -- this will make it so that there is no exception anywhere. Raising from 1 dimension is silly when you
        -- can start from two dimensions
