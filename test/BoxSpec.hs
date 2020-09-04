module BoxSpec (spec) where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import qualified Box               as B
import qualified BoxData           as D
import qualified Data.Map.Strict   as Map
import qualified Data.Set          as S
import qualified Orthotope         as O
import MapBuilder

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = do
  describe "Box" $ do
    describe "from string pair" $ do
      it "makes a box from a string pair" $ do
        let input = ("foo", "bar")
            expected = D.Box (O.Orthotope [O.Point "foo", O.Point "bar"]) (O.Point "foobar") (O.Point "bar") (O.Orthotope [O.Point "bar"]) (O.Orthotope [O.Point "foo"]) [S.singleton "foo", S.singleton "bar"]
        B.fromStringPair input `shouldBe` expected
    describe "combineAll" $ do
      it "attempts to combine all boxes with all boxes into the next dimension" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho (O.Point "foobar") (O.Point "bar") (O.Point "bar") (O.Point "bar") [S.singleton "foo", S.singleton "bar"]
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
            matchingOrtho = O.Orthotope [O.Point "baz", O.Point "bang"]
            matchingBox = D.Box matchingOrtho (O.Point "bazbang") (O.Point "bang") (O.Point "bar") (O.Point "bar") [S.singleton "baz", S.singleton "bang"]
            expected = D.Box (O.Orthotope [firstOrtho, matchingOrtho]) (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"]) (O.Orthotope [O.Point "bar", O.Point "bar"])  (O.Orthotope [O.Point "bar", O.Point "bar"]) [S.singleton "foo", S.fromList ["bar", "baz"], S.singleton "bang"]
        B.combineAll (Word wordMap) [firstBox, matchingBox] `shouldBe` [expected]
      it "attempts to combine all boxes with all boxes in the most recent dimension" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            secondOrtho = O.Orthotope [O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "is", O.Point "real"]]
            firstLines = O.Orthotope [O.Point "foobaz", O.Point "barbang"]
            firstColumn = O.Orthotope [O.Point "baz", O.Point "bang"]
            firstCenter1 = O.Orthotope [O.Orthotope [O.Point "bar"], O.Orthotope [O.Point "bang"]]
            firstCenter2 = O.Orthotope [O.Orthotope [O.Point "foo"], O.Orthotope [O.Point "baz"]]
            firstDiagonal = [S.singleton "foo", S.fromList ["bar", "baz"], S.singleton "bang"]
            firstBox = D.Box firstOrtho firstLines firstColumn firstCenter1 firstCenter2 firstDiagonal
            secondLines = O.Orthotope [O.Point "bazis", O.Point "bangreal"]
            secondColumn = O.Orthotope [O.Point "is", O.Point "real"]
            secondCenter1 = O.Orthotope [O.Orthotope [O.Point "bang"], O.Orthotope [O.Point "real"]]
            secondCenter2 = O.Orthotope [O.Orthotope [O.Point "bar"], O.Orthotope [O.Point "bang"]]
            secondDiagonal = [S.singleton "baz", S.fromList ["bang", "is"], S.singleton "real"]
            secondBox = D.Box secondOrtho secondLines secondColumn secondCenter1 secondCenter2 secondDiagonal
            expectedOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "is", O.Point "real"]]
            expectedLines = O.Orthotope [O.Point "foobazis", O.Point "barbangreal"]
            expectedColumn = secondColumn 
            expectedCenter1 = O.Orthotope [O.Orthotope [O.Point "bar"], O.Orthotope [O.Point "bang"], O.Orthotope [O.Point "real"]]
            expectedCenter2 = O.Orthotope [O.Orthotope [O.Point "foo"], O.Orthotope [O.Point "bar"], O.Orthotope [O.Point "bang"]]
            expectedDiagonal = [S.singleton "foo", S.fromList ["bar", "baz"], S.fromList ["bang", "is"], S.singleton "real"] 
            wordMap = Phrase $ Map.fromList [("foobaz", S.singleton "is"), ("barbang", S.singleton "real")]
            expectedBox = D.Box expectedOrtho expectedLines expectedColumn expectedCenter1 expectedCenter2 expectedDiagonal
        B.combineAll wordMap [firstBox, secondBox] `shouldBe` [expectedBox]
