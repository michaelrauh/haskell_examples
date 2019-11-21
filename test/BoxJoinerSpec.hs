module BoxJoinerSpec (spec) where

import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import qualified BoxJoiner         as B
import qualified Orthotope         as O

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = do
  describe "BoxJoiner" $ do
    describe "bottomLeftCorner" $ do
      it "selects the first bottom left corner for corner tracking" $ do
        B.bottomLeftCorner "a" "b" `shouldBe` "a"

    describe "topRightCorner" $ do
      it "selects the second top right corner for corner tracking" $ do
        B.topRightCorner "a" "b" `shouldBe` "b"

    describe "inLines" $ do
      it "combines single lines in dimension by concatenating" $ do
        let o1 = O.Orthotope [O.Point "a", O.Point "b"]
            o2 = O.Orthotope [O.Point "b", O.Point "c"]
            l1 = O.Point "ab"
            l2 = O.Point "bc"
        B.inLines o1 o2 l1 l2 `shouldBe` O.Point "abc"

      it "combines orthotopes in dimension by zipping and concatenating the first branch of the first orthotope with second lines" $ do
        let o1 = O.Orthotope [O.Orthotope [O.Point "a", O.Point "b"], O.Orthotope [O.Point "c", O.Point "d"]]
            o2 = O.Orthotope [O.Orthotope [O.Point "c", O.Point "d"], O.Orthotope [O.Point "e", O.Point "f"]]
            l1 = O.Orthotope [O.Point "ac", O.Point "bd"]
            l2 = O.Orthotope [O.Point "ce", O.Point "df"]
        B.inLines o1 o2 l1 l2 `shouldBe` O.Orthotope [O.Point "ace", O.Point "bdf"]

    describe "nextLines" $ do
      it "combines orthotopes cross dimension by zipping the orthotopes together" $ do
        let o1 = O.Orthotope [O.Orthotope [O.Point "a", O.Point "b"], O.Orthotope [O.Point "c", O.Point "d"]]
            o2 = O.Orthotope [O.Orthotope [O.Point "e", O.Point "f"], O.Orthotope [O.Point "g", O.Point "h"]]
            l1 = O.Orthotope [O.Point "ac", O.Point "bd"]
            l2 = O.Orthotope [O.Point "eg", O.Point "fh"]
        B.nextLines o1 o2 l1 l2 `shouldBe` O.Orthotope [O.Orthotope [O.Point "ae", O.Point "bf"], O.Orthotope [O.Point "cg", O.Point "dh"]]

    describe "inColumn" $ do
      it "selects the second column" $ do
        let o1 = O.Orthotope [O.Orthotope [O.Point "a", O.Point "b"], O.Orthotope [O.Point "c", O.Point "d"]]
            o2 = O.Orthotope [O.Orthotope [O.Point "e", O.Point "f"], O.Orthotope [O.Point "g", O.Point "h"]]
            col1 = O.Orthotope [O.Point "c", O.Point "d"]
            col2 = O.Orthotope [O.Point "g", O.Point "h"]
        B.inColumn o1 o2 col1 col2 `shouldBe` O.Orthotope [O.Point "g", O.Point "h"]

    describe "nextColumn" $ do
      it "selects the second column" $ do
        let o1 = O.Orthotope [O.Orthotope [O.Point "a", O.Point "b"], O.Orthotope [O.Point "c", O.Point "d"]]
            o2 = O.Orthotope [O.Orthotope [O.Point "e", O.Point "f"], O.Orthotope [O.Point "g", O.Point "h"]]
            col1 = O.Orthotope [O.Point "c", O.Point "d"]
            col2 = O.Orthotope [O.Point "g", O.Point "h"]
        B.nextColumn o1 o2 col1 col2 `shouldBe` O.Orthotope [O.Orthotope [O.Point "e", O.Point "f"], O.Orthotope [O.Point "g", O.Point "h"]]

    describe "inCenter" $ do
      it "takes the orthotope + of each center" $ do
        let oc1 = O.Orthotope [O.Point "a"]
            oc2 = O.Orthotope [O.Point "b"]
        B.inCenter oc1 oc2 `shouldBe` O.Orthotope [O.Point "a", O.Point "b"]

    describe "nextCenter" $ do
      it "takes the orthotope x of each center" $ do
        let oc1 = O.Orthotope [O.Point "a"]
            oc2 = O.Orthotope [O.Point "b"]
        B.nextCenter oc1 oc2 `shouldBe` O.Orthotope [O.Orthotope [O.Point "a"], O.Orthotope [O.Point "b"]]
