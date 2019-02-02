module Fold2
    ( execute2
    ) where

import Common
import Data.List
import qualified Data.Matrix as M

type Answer = (String, String, String, String, String)
type FormattedAnswer = (String, String, String, String)

matrixfy (a, b, d, c, _) = M.fromList 2 2 [a, b, c, d]

execute2 wordList uniqueWords =
  let answers = concatMap (filterFoldedWords . foldWord wordList) uniqueWords
  in map matrixfy $ nub answers

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