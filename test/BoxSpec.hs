module BoxSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import qualified Box as B
import qualified BoxData as D
import qualified Orthotope as O
import qualified Data.Map.Strict as Map
import qualified Data.Set as S

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = do
  describe "Box" $ do
    describe "from string pair" $ do
      it "makes a box from a string pair" $ do
        let input = ("foo", "bar")
            expected = D.Box (O.Orthotope [O.Point "foo", O.Point "bar"]) "foo" "bar" (O.Point "foobar") (O.Point "bar")
        B.fromStringPair input `shouldBe` expected

    describe "combineBoxes" $ do
      it "combines two boxes cross-dimensionally" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            secondOrtho = O.Orthotope [O.Orthotope [O.Point "sas", O.Point "quatch"], O.Orthotope [O.Point "is", O.Point "real"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
            secondBox = D.Box secondOrtho "sas" "real" (O.Orthotope [O.Point "sasis", O.Point "quatchreal"]) (O.Orthotope [O.Point "is", O.Point "real"])
            expected = D.Box (O.Orthotope [firstOrtho, secondOrtho]) "foo" "real" (O.Orthotope [O.Orthotope [O.Point "foosas", O.Point "barquatch"], O.Orthotope [O.Point "bazis", O.Point "bangreal"]]) secondOrtho
        B.combineBoxes (B.Next firstBox) (B.Next secondBox) `shouldBe` expected
      it "combines two boxes in the most recent dimension" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            secondOrtho = O.Orthotope [O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "is", O.Point "real"]]
            resultOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "is", O.Point "real"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
            secondBox = D.Box secondOrtho "sas" "real" (O.Orthotope [O.Point "bazis", O.Point "bangreal"]) (O.Orthotope [O.Point "is", O.Point "real"])
            expected = D.Box resultOrtho "foo" "real" (O.Orthotope [O.Point "foobazis", O.Point "barbangreal"]) (O.Orthotope [O.Point "is", O.Point "real"])
        B.combineBoxes (B.In firstBox) (B.In secondBox) `shouldBe` expected

    describe "getCenter1" $ do
      it "is the same as the column being tracked for the box" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
            expected = O.Orthotope [O.Point "baz", O.Point "bang"]
        B.getCenter1 firstBox `shouldBe` expected

    describe "getCenter2" $ do
      it "is the same as the head of the underlying orthotope for the box" $ do
        let firstOrtho = O.Orthotope [O.Orthotope [O.Point "foo", O.Point "bar"], O.Orthotope [O.Point "baz", O.Point "bang"]]
            firstBox = D.Box firstOrtho "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
            expected = O.Orthotope [O.Point "foo", O.Point "bar"]
        B.getCenter2 firstBox `shouldBe` expected

    describe "cornersDoNotMatch" $ do
      it "returns false when the bottom left corner of the first box matches the top right corner of the second box" $ do
        let firstBox = D.Box (O.Point "lol") "match" "notimportant" (O.Point "irrelevant") (O.Point "still")
            secondBox = D.Box (O.Point "no") "stillnot" "match" (O.Point "different") (O.Point "not the same")
        B.cornersDoNotMatch (B.Next firstBox) (B.Next secondBox) `shouldBe` False
      it "returns true otherwise" $ do
        let firstBox = D.Box (O.Point "lol") "no" "match" (O.Point "irrelevant") (O.Point "still")
            secondBox = D.Box (O.Point "no") "match" "match" (O.Point "different") (O.Point "not the same")
        B.cornersDoNotMatch (B.Next firstBox) (B.Next secondBox) `shouldBe` True

    describe "getPossibleNext" $ do
      it "returns a list of orthotopes by getting an orthotope out of the box, and mapping it across the wordMap" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho "wrong" "irrelevant" (O.Point "unrealistic") (O.Point "unrealistic")
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
        B.getPossibleNext wordMap (B.Next firstBox) `shouldBe` [O.Orthotope [O.Point "baz", O.Point "bang"], O.Orthotope [O.Point "biz", O.Point "bang"]]


    describe "getPossibleNextBoxes" $ do
      it "returns a list of known boxes containing the next orthotope for a given box, using a wordMap and a list of of all boxes of appropriate shape" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho "wrong" "irrelevant" (O.Point "unrealistic") (O.Point "unrealistic")
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
            matchingOrtho = O.Orthotope [O.Point "baz", O.Point "bang"]
            matchingBox = D.Box matchingOrtho "" "" (O.Point "") (O.Point "")
            nonMatchingOrtho = O.Orthotope [O.Point "not", O.Point "matching"]
            nonMatchingBox = D.Box nonMatchingOrtho "" "" (O.Point "") (O.Point "")
        B.getPossibleNextBoxes wordMap [B.Next nonMatchingBox, B.Next matchingBox] (B.Next firstBox) `shouldBe` [B.Next matchingBox]

    describe "eligibleToCombine" $ do
      it "returns a list of boxes that do not match corners with the input box" $ do
        let firstBox = D.Box (O.Point "foo")  "match" "" (O.Point "unrealistic") (O.Point "unrealistic")
            matchingBox = D.Box (O.Point "foo") "" "match" (O.Point "") (O.Point "")
            nonMatchingBox = D.Box (O.Point "foo") "diff" "erent" (O.Point "") (O.Point "")
        B.eligibleToCombine [B.Next nonMatchingBox, B.Next matchingBox] (B.Next firstBox) `shouldBe` [B.Next nonMatchingBox]

    describe "getNextEligibleBoxes" $ do
      it "returns all known boxes adjacent to the input box which don't have matching corners" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho "match" "" (O.Point "unrealistic") (O.Point "unrealistic")
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
            matchingOrtho = O.Orthotope [O.Point "baz", O.Point "bang"]
            matchingBox = D.Box matchingOrtho "diff" "stilldiff" (O.Point "") (O.Point "")
            matchingBoxWithMatchingCorners = D.Box matchingOrtho "" "match" (O.Point "") (O.Point "")
            nonMatchingOrtho = O.Orthotope [O.Point "not", O.Point "matching"]
            nonMatchingBox = D.Box nonMatchingOrtho "" "" (O.Point "") (O.Point "")
        B.getNextEligibleBoxes wordMap [B.Next nonMatchingBox, B.Next matchingBox, B.Next matchingBoxWithMatchingCorners] (B.Next firstBox) `shouldBe` [B.Next matchingBox]

    describe "combine" $ do
      it "searches for adjacent eligible boxes and combines with them cross-dimensionally for a given box" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho "foo" "bar" (O.Point "foobar") (O.Point "bar")
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
            matchingOrtho = O.Orthotope [O.Point "baz", O.Point "bang"]
            matchingBox = D.Box matchingOrtho "baz" "bang" (O.Point "bazbang") (O.Point "bang")
            expected = D.Box (O.Orthotope [firstOrtho, matchingOrtho]) "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
        B.combine wordMap [B.Next matchingBox] (B.Next firstBox) `shouldBe` [expected]

    describe "combineAll" $ do
      it "attempts to combine all boxes with all boxes into the next dimension" $ do
        let firstOrtho = O.Orthotope [O.Point "foo", O.Point "bar"]
            firstBox = D.Box firstOrtho "foo" "bar" (O.Point "foobar") (O.Point "bar")
            wordMap = Map.fromList [("foo", S.fromList["baz", "biz"]), ("bar", S.singleton "bang")]
            matchingOrtho = O.Orthotope [O.Point "baz", O.Point "bang"]
            matchingBox = D.Box matchingOrtho "baz" "bang" (O.Point "bazbang") (O.Point "bang")
            expected = D.Box (O.Orthotope [firstOrtho, matchingOrtho]) "foo" "bang" (O.Orthotope [O.Point "foobaz", O.Point "barbang"]) (O.Orthotope [O.Point "baz", O.Point "bang"])
        B.combineAll wordMap [B.Next firstBox, B.Next matchingBox] `shouldBe` [expected]
