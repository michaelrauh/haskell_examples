module FoldSquare (foldSquare) where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import MapBuilder
import MatrixUtils
import CombineNextDimension

type Answer = (String, String, String, String, String)
type StringMatrix = M.Matrix String
type MapToSet = Map.Map String (S.Set String)

foldSquare :: [String] -> [String] -> [Box]
foldSquare wordList uniqueWords =
  let nextMap = buildNextWordMap wordList
      prevMap = buildPreviousWordMap wordList
      answers = concatMap (filterFoldedWords . foldWord nextMap prevMap) uniqueWords
      finalMatrices = removeEquivalentMatrices $ map matrixfy answers
  in map Square finalMatrices

foldWord :: MapToSet -> MapToSet -> String -> [Answer]
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

removeEquivalentMatrices :: [StringMatrix] -> [StringMatrix]
removeEquivalentMatrices = map head . groupBy isTranspose

matrixfy :: Answer -> StringMatrix
matrixfy (a, b, d, c, _) = M.fromList 2 2 [a, b, c, d]
