module FoldSquare
    ( foldSquare
    ) where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import MapBuilder

type Answer = (String, String, String, String, String)
type FormattedAnswer = (String, String, String, String)

foldSquare :: Foldable t => [String] -> t String -> [M.Matrix String]
foldSquare wordList uniqueWords =
  let nextMap = buildNextWordMap wordList
      prevMap = buildPreviousWordMap wordList
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

matrixfy :: (a, a, a, a, e) -> M.Matrix a
matrixfy (a, b, d, c, _) = M.fromList 2 2 [a, b, c, d]
