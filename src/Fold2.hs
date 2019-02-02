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
  let nextMap = buildMap wordList
      prevMap = buildReverseMap wordList
      answers = concatMap (filterFoldedWords . foldWord nextMap prevMap) uniqueWords
  in map matrixfy $ nub answers

foldWord wordMap prevMap a =
  let nextWords' = nextWords wordMap
      prevWords = nextWords prevMap
  in
  do
      b <- nextWords' a
      d <- nextWords' b
      c <- prevWords d
      a' <- prevWords c
      return (a, b, d, c, a')

filterFoldedWords :: [Answer] -> [Answer]
filterFoldedWords = filter(\(a, b, _, c, a') -> b /= c && a == a')
