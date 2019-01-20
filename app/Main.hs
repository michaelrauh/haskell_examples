module Main where

import Lib

import Data.List
import Control.Monad

type Answer = (String, String, String, String, String)
type FilteredAnswer = (String, String, String, String)

main :: IO ()
main = do
  contents <- getContents
  putStr $ concat (execute contents)

adjacentWords :: Int -> [String] -> String -> [String]
adjacentWords offset wordList word =
      let indices = elemIndices word wordList
          offsetIndices = map (+ offset) indices
          remaining = filter (liftM2 (&&) (> 0) (< length wordList)) offsetIndices
  in map (wordList !!) remaining

foldWord :: [String] -> String -> [Answer]
foldWord wordList a =
  let nextWords = adjacentWords 1 wordList
      prevWords = adjacentWords (-1) wordList
  in
  do
      b <- nextWords a
      d <- nextWords b
      c <- prevWords d
      a' <- prevWords c
      return (a, b, d, c, a')

filterFoldedWords :: [Answer] -> [Answer]
filterFoldedWords = filter(\(a, b, _, c, a') -> b /= c && a == a')

formFinalAnswer :: Answer -> FilteredAnswer
formFinalAnswer (a, b, d, c, _) = (a, b, d, c)

prettyPrint :: FilteredAnswer -> String
prettyPrint (a, b, d, c) = a  ++ " " ++ b ++ "\n" ++ c ++ " " ++ d ++ "\n\n"

execute :: String -> [String]
execute input =
  let wordList = words input
      uniqueWords = nub wordList
      answers = concatMap (filterFoldedWords . foldWord wordList) uniqueWords
      formattedAnswers = map formFinalAnswer $ nub answers
  in map prettyPrint formattedAnswers
