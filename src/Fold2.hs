module Fold2
    ( produceResult,
    execute2
    ) where

import Common
import Data.List

type Answer = (String, String, String, String, String)
type FormattedAnswer = (String, String, String, String)

produceResult :: [FormattedAnswer] -> String
produceResult = concatMap prettyPrint

execute2 :: [String] -> [FormattedAnswer]
execute2 wordList =
  let uniqueWords = nub wordList
      answers = concatMap (filterFoldedWords . foldWord wordList) uniqueWords
      formattedAnswers = map formFinalAnswer $ nub answers
  in formattedAnswers

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

formFinalAnswer :: Answer -> FormattedAnswer
formFinalAnswer (a, b, d, c, _) = (a, b, d, c)

prettyPrint :: FormattedAnswer -> String
prettyPrint (a, b, d, c) = a  ++ " " ++ b ++ "\n" ++ c ++ " " ++ d ++ "\n\n"
