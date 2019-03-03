module MatrixUtilsSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "matrix utils" $ do
    it "does not exist" $ do
      True `shouldBe` False
