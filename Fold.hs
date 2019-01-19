import Data.List
import Control.Monad
import qualified Data.Set as Set

type Answer = (String, String, String, String, String)

main :: IO ()
main = do
  contents <- getContents
  print $ execute contents

adjacentWords :: Int -> String -> String -> [String]
adjacentWords offset corpus word =
  let wordList = words corpus
      indices = elemIndices word wordList
      offsetIndices = map (+ offset) indices
      remaining = filter (liftM2 (&&) (> 0) (< length wordList)) offsetIndices
  in map (wordList !!) remaining

foldWord :: String -> String -> [(String, String, String, String, String)]
foldWord corpus a =
  let nextWords = adjacentWords 1 corpus
      prevWords = adjacentWords (-1) corpus
  in
  do
      b <- nextWords a
      d <- nextWords b
      c <- prevWords d
      a' <- prevWords c
      return (a, b, d, c, a')

filterFoldedWords :: [Answer] -> [Answer]
filterFoldedWords = filter(\(a, b, _, c, a') -> b /= c && a == a')

execute :: String -> Set.Set Answer
execute input =
  let foldCorpus = foldWord input
      uniqueWords = Set.fromList $ words input
  in Set.fromList $ concatMap (filterFoldedWords . foldCorpus) uniqueWords
