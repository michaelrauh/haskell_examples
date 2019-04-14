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
            expected = D.Box (O.Orthotope [O.Point "foo", O.Point "bar"]) "foo" "bar" (O.Point "foobar") (O.Point "bar")
        B.fromStringPair input `shouldBe` expected

    describe "upDimension" $ do
      it "combines two boxes cross-dimensionally" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            secondOrtho = O.Orthotope [O.Orthotope [O.Point "sas", O.Point "quatch"], O.Orthotope [O.Point "is", O.Point "real"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
            secondBox = D.Box secondOrtho "sas" "real" (O.Orthotope [O.Point "sasis", O.Point "quatchreal"]) (O.Orthotope [O.Point "is", O.Point "real"])
            expected = D.Box (O.Orthotope [firstOrtho, secondOrtho]) "foo" "real" (O.Orthotope [O.Orthotope [O.Point "foosas", O.Point "barquatch"], O.Orthotope [O.Point "bazis", O.Point "bangreal"]]) secondOrtho
        B.upDimension firstBox secondBox `shouldBe` expected

    describe "addLength" $ do
      it "combines two boxes in the most recently added dimension" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            secondOrtho = O.Orthotope [O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "is", O.Point "real"]]
            resultOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "is", O.Point "real"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
            secondBox = D.Box secondOrtho "sas" "real" (O.Orthotope [O.Point "bazis", O.Point "bangreal"]) (O.Orthotope [O.Point "is", O.Point "real"])
            expected = D.Box resultOrtho "foo" "real" (O.Orthotope [O.Point "foobazis", O.Point "barbangreal"]) (O.Orthotope [O.Point "is", O.Point "real"])
        B.addLength firstBox secondBox `shouldBe` expected

    describe "getCenter1" $ do
      it "is the same as the column being tracked for the box" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
            expected = O.Orthotope [O.Point "baz", O.Point "bang"]
        B.getCenter1 firstBox `shouldBe` expected

    describe "getCenter2" $ do
      it "is the same as the column being tracked for the box" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
            expected = O.Orthotope [O.Point "foo", O.Point "bar"]
        B.getCenter2 firstBox `shouldBe` expected
