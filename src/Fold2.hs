module Fold2
    ( execute2
    ) where

import Common
import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map

type Answer = (String, String, String, String, String)
type FormattedAnswer = (String, String, String, String)

matrixfy :: (a, a, a, a, e) -> M.Matrix a
matrixfy (a, b, d, c, _) = M.fromList 2 2 [a, b, c, d]

execute2 :: Foldable t => [String] -> t String -> [M.Matrix String]
execute2 wordList uniqueWords =
  let nextMap = buildMap wordList
      prevMap = buildReverseMap wordList
      answers = concatMap (filterFoldedWords . foldWord nextMap prevMap) uniqueWords
  in map matrixfy answers

foldWord :: Ord e => Map.Map e (S.Set e) -> Map.Map e (S.Set e) -> e -> [(e, e, e, e, e)]
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

buildSlidingTuple :: [a] -> [(a, S.Set a)]
buildSlidingTuple [] = []
buildSlidingTuple [first] = []
buildSlidingTuple [first, second] = [(first, S.singleton second)]
buildSlidingTuple (first:second:rest) = (first, S.singleton second) : buildSlidingTuple (second : rest)

buildMap :: Ord a => [a] -> Map.Map a (S.Set a)
buildMap wordList = Map.fromListWith S.union $ buildSlidingTuple wordList

reverseTuple :: (a1, S.Set a2) -> (a2, S.Set a1)
reverseTuple (first, second) = (unwrapSingleton second, S.singleton first)

unwrapSingleton :: S.Set a -> a
unwrapSingleton s = head $ S.elems s

buildReverseSlidingTuple :: [(a1, S.Set a2)] -> [(a2, S.Set a1)]
buildReverseSlidingTuple = map reverseTuple

buildReverseMap :: Ord a => [a] -> Map.Map a (S.Set a)
buildReverseMap wordList = Map.fromListWith S.union $ buildReverseSlidingTuple $ buildSlidingTuple wordList
