import Data.List
import Control.Monad
import qualified Data.Set as Set

type Answer = (String, String, String, String, String)

example :: String
example = "a b c d a b a b a c a a b c d a b"

adjacentWords :: Int -> String -> String -> [String]
adjacentWords offset corpus word =
  let wordList = words corpus
      indices = elemIndices word wordList
      offsetIndices = map (+ offset) indices
      remaining = filter (liftM2 (&&) (> 0) (< length wordList)) offsetIndices
  in map (wordList !!) remaining

nextWords :: String -> [String]
nextWords = adjacentWords 1 example

prevWords :: String -> [String]
prevWords = adjacentWords (-1) example

foldWord :: String -> [Answer]
foldWord a = do
      b <- nextWords a
      d <- nextWords b
      c <- prevWords d
      a' <- prevWords c
      return (a, b, d, c, a')

filterFoldedWords :: [Answer] -> [Answer]
filterFoldedWords = filter(\(a, b, _, c, a') -> b /= c && a == a')

exampleWordList :: [String]
exampleWordList = words example

execute :: Set.Set Answer
execute = Set.fromList $ concatMap (filterFoldedWords . foldWord) exampleWordList

main :: IO ()
main = print execute
