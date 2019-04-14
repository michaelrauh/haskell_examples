module OrthotopeSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Orthotope
import MapBuilder

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
      it "applies a function to all points inside an orthotope" $ do
        fmap succ (Orthotope [Point 1, Point 2]) `shouldBe` Orthotope [Point 2, Point 3]
      it "applies a function to nested orthotopes" $ do
        fmap succ (Orthotope [Orthotope [Point 1, Point 2], Orthotope [Point 3]]) `shouldBe` Orthotope [Orthotope [Point 2, Point 3], Orthotope [Point 4]]
    describe "folding orthotopes" $ do
      it "produces a single value that is in folding order" $ do
        concat (Orthotope [Orthotope [Point "a"], Point "b", Point "c", Orthotope [Point "d", Point "e"]]) `shouldBe` "abcde"
    describe "traversing orthotopes" $ do
      it "allows mapping monadically" $ do
        mapM (\x -> [x, x + 1]) (Orthotope [Point 5, Point 6]) `shouldBe` [Orthotope [Point 5,Point 6],Orthotope [Point 5,Point 7],Orthotope [Point 6,Point 6],Orthotope [Point 6,Point 7]]
    describe "addlength" $ do
      it "combines two orthotopes by taking the head of one orthotope and making it the head of the other" $ do
        addLength (Orthotope [Point "a", Point "b"]) (Orthotope [Point "c", Point "d"]) `shouldBe` Orthotope [Point "a", Point "c", Point "d"]
    describe "getNext" $ do
      it "gets all possible mixes of next words for each position in the orthotope" $ do
        let fromOrtho = Orthotope [Point "a", Point "b"]
            wordList = ["a", "c", "b", "d", "a", "e"]
            wordMap = buildNextWordMap wordList
            expected = [Orthotope [Point "c", Point "d"], Orthotope [Point "e", Point "d"]]
            actual = getNext wordMap fromOrtho
        actual `shouldBe` expected
    describe "zipWithOrtho" $ do
      it "applies a binary function to two points" $ do
        zipWithOrtho (+) (Point 3) (Point 4) `shouldBe` Point 7
      it "applies a binary function to two orthotopes that are the same exact shape" $ do
        zipWithOrtho (+) (Orthotope [Point 3, Point 7]) (Orthotope [Point 11, Point 13]) `shouldBe` Orthotope [Point 14, Point 20]
