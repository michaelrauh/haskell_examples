module CombineVertical
    ( combineVertical
    ) where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import MapBuilder
import MatrixUtils
import Control.Monad

type MatrixPair = (Matrix, Matrix)
type Matrix = M.Matrix String

combineVertical = combine getBottomRowList (M.<->)

combine getEdgeOfMatrix matrixCombineOperator phraseMap inputMatrices =
  let possiblePairs = findPossiblePairs inputMatrices
      answers = filterPairs getEdgeOfMatrix possiblePairs phraseMap
      final = map (combineMatrixPair matrixCombineOperator) answers
  in nub final

findPossiblePairs inputMatrices = liftM2 (,) inputMatrices inputMatrices

filterCandidates :: MatrixPair -> Bool
filterCandidates pair =
  cornersDoNotMatch pair && centersOverlapVertically pair

cornersDoNotMatch :: MatrixPair -> Bool
cornersDoNotMatch (first, second) =
  getBottomLeft first /= getTopRight second

centersOverlapVertically (top, bottom) =
  removeTopRow top == removeBottomRow bottom

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

filterFoldable getEdgeOfMatrix phraseMap (first, second) =
  let nextWords' = nextWords phraseMap
      froms = getFroms first
      possibilities = getPossibilities froms phraseMap
      correspondences = getZips getEdgeOfMatrix possibilities second
      answers = getAnswers correspondences
  in checkAnswers answers

getFroms = getColumns
getPossibilities m phraseMap = map (nextWords phraseMap) m
getZips getEdgeOfMatrix possibilities m = zip (getEdgeOfMatrix m) possibilities
getAnswers = map wordInList
checkAnswers = and

filterPairs getEdgeOfMatrix matrixPairs phraseMap =
  let candidates = filter filterCandidates matrixPairs
  in  filter (filterFoldable getEdgeOfMatrix phraseMap) candidates

combineMatrixPair matrixCombineOperator (first, second) = first `matrixCombineOperator` getBottomRow second
