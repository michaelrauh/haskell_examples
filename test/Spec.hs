import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck
import Fold2

main :: IO ()
main = hspec $ do
  describe "execute2" $ do
    it "folds a list of words into a formatted answer" $ do
      execute2 ["a", "b", "c", "d", "a", "c", "b", "d"] `shouldBe` [("a", "b", "d", "c")]
