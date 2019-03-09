module CombineHorizontal
    ( combineHorizontal
    ) where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import MapBuilder
import MatrixUtils
import Control.Monad

type StringMatrix = M.Matrix String

type MatrixPair = (Matrix, Matrix)
type Matrix = M.Matrix String

combineHorizontal = combine getRightColumnList (M.<|>) getRows

combine getEdgeOfMatrix matrixCombineOperator matrixSlicingOperator phraseMap inputMatrices =
  let possiblePairs = findPossiblePairs inputMatrices
      answers = filterPairs getEdgeOfMatrix matrixSlicingOperator possiblePairs phraseMap
      final = map (combineMatrixPair matrixCombineOperator) answers
  in nub final

findPossiblePairs inputMatrices = liftM2 (,) inputMatrices inputMatrices

filterCandidates :: MatrixPair -> Bool
filterCandidates pair =
  cornersDoNotMatch pair && centersOverlap pair

cornersDoNotMatch :: MatrixPair -> Bool
cornersDoNotMatch (first, second) =
  getBottomLeft first /= getTopRight second

centersOverlap (left, right) =
  removeLeftColumn left == removeRightColumn right

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

filterFoldable getEdgeOfMatrix matrixSlicingOperator phraseMap (first, second) =
  let nextWords' = nextWords phraseMap
      froms = matrixSlicingOperator first
      possibilities = getPossibilities froms phraseMap
      correspondences = getZips getEdgeOfMatrix possibilities second
      answers = getAnswers correspondences
  in checkAnswers answers

getPossibilities m phraseMap = map (nextWords phraseMap) m
getZips getEdgeOfMatrix possibilities m = zip (getEdgeOfMatrix m) possibilities
getAnswers = map wordInList
checkAnswers = and

filterPairs getEdgeOfMatrix matrixSlicingOperator matrixPairs phraseMap =
  let candidates = filter filterCandidates matrixPairs
  in  filter (filterFoldable getEdgeOfMatrix matrixSlicingOperator phraseMap) candidates

combineMatrixPair :: (Matrix -> Matrix -> Matrix) -> MatrixPair -> Matrix
combineMatrixPair matrixCombineOperator (first, second) = first `matrixCombineOperator` getRightColumn second
