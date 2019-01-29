import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck
import Fold2
import Fold3
import Fold4
import qualified Data.Matrix as M

main :: IO ()
main = hspec $ do
  describe "execute2" $ do
    it "folds a list of words into a formatted answer" $ do
      execute2 ["a", "b", "c", "d", "a", "c", "b", "d"] ["a", "b", "c", "d"] `shouldBe` [M.fromList 2 2 ["a","b","c","d"]]
