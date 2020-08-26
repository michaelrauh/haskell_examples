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

    describe "combineBoxes" $ do
      it "combines two boxes cross-dimensionally" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            secondOrtho = O.Orthotope [O.Orthotope [O.Point "sas", O.Point "quatch"], O.Orthotope [O.Point "is", O.Point "real"]]
            firstLines = O.Orthotope [O.Point "foobaz", O.Point "barbang"]
            firstColumn = O.Orthotope [O.Point "baz", O.Point "bang"]
            firstCenter1 = O.Orthotope [O.Orthotope [O.Point "bar"], O.Orthotope [O.Point "bang"]]
            firstCenter2 = O.Orthotope [O.Orthotope [O.Point "foo"], O.Orthotope [O.Point "baz"]]
            firstDiagonal = [S.singleton "foo", S.fromList ["bar", "baz"], S.singleton "bang"]
            firstBox = D.Box firstOrtho firstLines firstColumn firstCenter1 firstCenter2 firstDiagonal
            secondLines = O.Orthotope [O.Point "sasis", O.Point "quatchreal"]
            secondColumn = O.Orthotope [O.Point "is", O.Point "real"]
            secondCenter1 = O.Orthotope [O.Orthotope [O.Point "quatch"], O.Orthotope [O.Point "real"]]
            secondCenter2 = O.Orthotope [O.Orthotope [O.Point "sas"], O.Orthotope [O.Point "is"]]
            secondDiagonal = [S.singleton "sas", S.fromList ["quatch", "is"], S.singleton "real"]
            secondBox = D.Box secondOrtho secondLines secondColumn secondCenter1 secondCenter2 secondDiagonal
            expectedOrtho = O.Orthotope [firstOrtho, secondOrtho]
            expectedLines = O.Orthotope [O.Orthotope [O.Point "foosas", O.Point "barquatch"], O.Orthotope [O.Point "bazis", O.Point "bangreal"]]
            expectedColumn = secondOrtho
            expectedCenter1 = O.Orthotope [firstCenter1, secondCenter1]
            expectedCenter2 = O.Orthotope [firstCenter2, secondCenter2]
            expectedDiagonal = [S.singleton "foo", S.fromList ["bar", "baz", "sas"], S.fromList ["bang", "quatch", "is"], S.singleton "real"]
            expectedBox = D.Box expectedOrtho expectedLines expectedColumn expectedCenter1 expectedCenter2 expectedDiagonal
        B.combineBoxesNext firstBox secondBox `shouldBe` expectedBox 
      it "combines two boxes in the most recent dimension" $ do
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
            secondCenter2 = O.Orthotope [O.Orthotope [O.Point "baz"], O.Orthotope [O.Point "is"]]
            secondDiagonal = [S.singleton "baz", S.fromList ["bang", "is"], S.singleton "real"]
            secondBox = D.Box secondOrtho secondLines secondColumn secondCenter1 secondCenter2 secondDiagonal
            expectedOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "is", O.Point "real"]]
            expectedLines = O.Orthotope [O.Point "foobazis", O.Point "barbangreal"]
            expectedColumn = secondColumn 
            expectedCenter1 = O.Orthotope [O.Orthotope [O.Point "bar"], O.Orthotope [O.Point "bang"], O.Orthotope [O.Point "real"]]
            expectedCenter2 = O.Orthotope [O.Orthotope [O.Point "foo"], O.Orthotope [O.Point "baz"], O.Orthotope [O.Point "is"]]
            expectedDiagonal = [S.singleton "foo", S.fromList ["bar", "baz"], S.fromList ["bang", "is"], S.singleton "real"] 
            expectedBox = D.Box expectedOrtho expectedLines expectedColumn expectedCenter1 expectedCenter2 expectedDiagonal
        B.combineBoxesIn firstBox secondBox `shouldBe` expectedBox 
    describe "eligible next" $ do
      it "returns false if there would have been repeats in the new diagonal sets" $ do
        let firstBox = D.Box (O.Point "lol") (O.Point "irrelevant") (O.Point "still") (O.Point "still no") (O.Point "also no") [S.singleton "unchecked", S.fromList ["match", "different"]]
            secondBox = D.Box (O.Point "no") (O.Point "different") (O.Point "not the same") (O.Point "still no") (O.Point "also no") [S.fromList ["checkedbutdiff", "match"], S.singleton "not-checked"]
        B.eligibleNext firstBox secondBox `shouldBe` False
      it "returns true otherwise" $ do
        let firstBox = D.Box (O.Point "lol") (O.Point "irrelevant") (O.Point "still") (O.Point "still no") (O.Point "also no") [S.singleton "unchecked", S.fromList ["no-match", "different"]]
            secondBox = D.Box (O.Point "no") (O.Point "different") (O.Point "not the same") (O.Point "still no") (O.Point "also no") [S.fromList ["checkedbutdiff", "not-a-match"], S.singleton "not-checked"]
        B.eligibleNext firstBox secondBox `shouldBe` True
    describe "eligible in" $ do
      it "returns false when center1 of the first box does not match center2 of the second box" $ do
        let firstBox = D.Box (O.Point "lol") (O.Point "irrelevant") (O.Point "still") (O.Point "center1") (O.Point "also no") []
            secondBox = D.Box (O.Point "no") (O.Point "different") (O.Point "not the same") (O.Point "still no") (O.Point "center2") []
        B.eligibleIn firstBox secondBox `shouldBe` False
      it "returns true otherwise" $ do
        let firstBox = D.Box (O.Point "lol") (O.Point "irrelevant") (O.Point "still") (O.Point "match center") (O.Point "also no") []
            secondBox = D.Box (O.Point "no") (O.Point "different") (O.Point "not the same") (O.Point "still no") (O.Point "match center") []
        B.eligibleIn firstBox secondBox `shouldBe` True
    describe "getPossibleNext for next dimension" $ do
      it "returns a list of orthotopes by getting an orthotope out of the box, and mapping it across the wordMap" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho (O.Point "unrealistic") (O.Point "unrealistic") (O.Point "center1") (O.Point "center2") []
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
        B.getPossibleNext (Word wordMap) firstBox `shouldBe` S.fromList [O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "biz", O.Point "bang"]]
    describe "getPossibleNext for current dimension" $ do
      it "returns a list of orthotopes by getting lines out of the box, and mapping it across the wordMap" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstLines = O.Orthotope [O.Point "one", O.Point "two"]
            firstBox = D.Box firstOrtho firstLines (O.Point "unrealistic") (O.Point "center1") (O.Point "center2") []
            wordMap = Map.fromList [("one", S.fromList["baz", "biz"]), ("two", S.singleton "bang")]
        B.getPossibleNext (Phrase wordMap) firstBox `shouldBe` S.fromList [O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "biz", O.Point "bang"]]
    describe "getPossibleNextBoxes" $ do
      it "returns a list of known boxes containing the next orthotope for a given box, using a wordMap and a list of of all boxes of appropriate shape" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho (O.Point "unrealistic") (O.Point "unrealistic") (O.Point "center1") (O.Point "center2") []
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
            matchingOrtho = O.Orthotope [O.Point "baz", O.Point "bang"]
            matchingBox = D.Box matchingOrtho (O.Point "") (O.Point "") (O.Point "center1") (O.Point "center2") []
            nonMatchingOrtho = O.Orthotope [O.Point "not", O.Point "matching"]
            nonMatchingBox = D.Box nonMatchingOrtho (O.Point "") (O.Point "") (O.Point "center1") (O.Point "center2") []
        B.getPossibleNextBoxes (Word wordMap) [nonMatchingBox, matchingBox] firstBox `shouldBe` [matchingBox]
    describe "eligibleToCombineNext" $ do
      it "returns a list of boxes that would not form repeating diagonals" $ do
        let firstBox = D.Box (O.Point "foo")  (O.Point "unrealistic") (O.Point "unrealistic") (O.Point "") (O.Point "") [S.singleton "notchecked", S.fromList ["ok", "yarp"]]
            matchingBox = D.Box (O.Point "foo") (O.Point "") (O.Point "") (O.Point "") (O.Point "") [S.fromList ["diff", "narp"], S.singleton "nope"]
            nonMatchingBox = D.Box (O.Point "foo") (O.Point "") (O.Point "") (O.Point "") (O.Point "") [S.fromList ["ok", "yarp"], S.singleton "other"]
        B.eligibleToCombineNext [nonMatchingBox, matchingBox] firstBox `shouldBe` [matchingBox]
    describe "getNextEligibleBoxes" $ do
      it "returns all known boxes adjacent to the input box which are considered eligible" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho (O.Point "unrealistic") (O.Point "unrealistic") (O.Point "center1") (O.Point "center2") [S.singleton "notchecked", S.fromList ["ok", "yarp"]]
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
            matchingOrtho = O.Orthotope [O.Point "baz", O.Point "bang"]
            matchingBox = D.Box matchingOrtho (O.Point "") (O.Point "") (O.Point "center1") (O.Point "center2") [S.fromList ["diff", "narp"], S.singleton "nope"]
            matchingBoxWithMatchingCorners = D.Box matchingOrtho (O.Point "") (O.Point "") (O.Point "center1") (O.Point "center2") [S.fromList ["ok", "yarp"], S.singleton "notchecked"]
            nonMatchingOrtho = O.Orthotope [O.Point "not", O.Point "matching"]
            nonMatchingBox = D.Box nonMatchingOrtho (O.Point "") (O.Point "") (O.Point "center1") (O.Point "center2") [S.fromList ["ok", "yarp"], S.singleton "other"]
        B.getNextEligibleBoxes (Word wordMap) [nonMatchingBox, matchingBox, matchingBoxWithMatchingCorners] firstBox `shouldBe` [matchingBox]
    describe "combine" $ do
     it "searches for adjacent eligible boxes and combines with them cross-dimensionally for a given box" $ do
       let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
           firstBox = D.Box firstOrtho (O.Point "foobar") (O.Point "bar") (O.Point "") (O.Point "") [S.singleton "foo", S.singleton "bar"]
           wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
           matchingOrtho = O.Orthotope [O.Point "baz", O.Point "bang"]
           matchingBox = D.Box matchingOrtho (O.Point "bazbang") (O.Point "bang") (O.Point "") (O.Point "") [S.singleton "baz", S.singleton "bang"]
           expected = D.Box (O.Orthotope [firstOrtho, matchingOrtho]) (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])  (O.Orthotope [O.Point "", O.Point ""]) (O.Orthotope [O.Point "", O.Point ""]) [S.singleton "foo", S.fromList ["bar", "baz"], S.singleton "bang"]
       B.combine (Word wordMap) [matchingBox] firstBox `shouldBe` [expected]
    describe "combineAll" $ do
      it "attempts to combine all boxes with all boxes into the next dimension" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho (O.Point "foobar") (O.Point "bar") (O.Point "bar") (O.Point "bar") [S.singleton "foo", S.singleton "bar"]
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
            matchingOrtho = O.Orthotope [O.Point "baz", O.Point "bang"]
            matchingBox = D.Box matchingOrtho (O.Point "bazbang") (O.Point "bang") (O.Point "bar") (O.Point "bar") [S.singleton "baz", S.singleton "bang"]
            expected = D.Box (O.Orthotope [firstOrtho, matchingOrtho]) (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"]) (O.Orthotope [O.Point "bar", O.Point "bar"])  (O.Orthotope [O.Point "bar", O.Point "bar"]) [S.singleton "foo", S.fromList ["bar", "baz"], S.singleton "bang"]
        B.combineAll (Word wordMap) [firstBox, matchingBox] `shouldBe` [expected]
