import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck
import Fold2
import FoldHorizontal
import qualified Data.Matrix as M

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
  describe "execute2" $ do
    it "folds a list of words into a formatted answer" $ do
      let expected2x2 = [M.fromList 2 2 ["a","b","c","d"], M.fromList 2 2 ["a","c","b","d"]]
          answers2x2 = execute2 ["a", "b", "c", "d", "a", "c", "b", "d"] ["a", "b", "c", "d"]
      answers2x2 `shouldBe` expected2x2
      
  describe "executeHorizontal" $ do
    it "combines answers horizontally to form wider answers" $ do
      --  a b     b c      a b c
      --  d e  +  e f  ->  d e f
      let inputCorpus = ["a", "b", "c", "d", "e", "f", "a", "d", "b", "e", "c", "f"]
          inputMatrices = [M.fromList 2 2 ["a", "b", "d", "e"], M.fromList 2 2 ["b", "c", "e", "f"]]
          expectedMatrices = [M.fromList 2 3 ["a", "b", "c", "d", "e", "f"]]
      executeHorizontal inputCorpus inputMatrices `shouldBe` expectedMatrices
