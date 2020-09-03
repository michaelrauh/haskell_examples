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
