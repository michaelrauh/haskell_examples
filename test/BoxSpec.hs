module BoxSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import qualified Box as B
import qualified BoxData as D
import qualified Orthotope as O

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Box" $ do
    describe "from string pair" $ do
      it "makes a box from a string pair" $ do
        let input = ("foo", "bar")
            expected = D.Box (O.Orthotope [O.Point "foo", O.Point "bar"]) "foo" "bar" (O.Point "bar") (O.Point "foobar")
        B.fromStringPair input `shouldBe` expected

    describe "upDimension" $ do
      it "combines two boxes cross-dimensionally" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            secondOrtho = O.Orthotope [O.Orthotope [O.Point "sas", O.Point "quatch"], O.Orthotope [O.Point "is", O.Point "real"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "bar", O.Point "bang"]) (O.Orthotope [O.Point "foobar", O.Point "bazbang"])
            secondBox = D.Box secondOrtho "sas" "real" (O.Orthotope [O.Point "quatch", O.Point "real"]) (O.Orthotope [O.Point "sasquatch", O.Point "isreal"])
            expected = D.Box (O.Orthotope [firstOrtho, secondOrtho]) "foo" "real" (O.Orthotope [O.Orthotope [O.Point "bar", O.Point "bang"], O.Orthotope [O.Point "quatch", O.Point "real"]]) (O.Orthotope [O.Orthotope [O.Point "foobar", O.Point "bazbang"], O.Orthotope [O.Point "sasquatch", O.Point "isreal"]])
        B.upDimension firstBox secondBox `shouldBe` expected

    describe "addLength" $ do
      it "combines two boxes in the most recently added dimension" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            secondOrtho = O.Orthotope [O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "is", O.Point "real"]]
            resultOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "is", O.Point "real"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "bar", O.Point "bang"]) (O.Orthotope [O.Point "foobar", O.Point "bazbang"])
            secondBox = D.Box secondOrtho "baz" "real" (O.Orthotope [O.Point "bang", O.Point "real"]) (O.Orthotope [O.Point "bazbang", O.Point "isreal"])
            expected = D.Box resultOrtho "foo" "real" (O.Orthotope [O.Point "bar", O.Point "bang", O.Point "real"]) (O.Orthotope [O.Point "foobar", O.Point "bazbang", O.Point "isreal"])
        B.addLength firstBox secondBox `shouldBe` expected
