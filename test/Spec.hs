import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
  describe "failure" $ do
    it "fails" $ do
      True `shouldBe` False
