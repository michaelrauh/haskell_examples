module Combine (combineHorizontal, combineVertical) where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import MapBuilder
import MatrixUtils
import Control.Applicative

type MatrixPair = (Matrix, Matrix)
type Matrix = M.Matrix String

combineHorizontal = combine getRightColumnList getRows centersOverlapHorizontally combineMatrixPairHorizontally
combineVertical = combine getBottomRowList getColumns centersOverlapVertically combineMatrixPairVertically

combine getEdgeOfMatrix matrixSlicingOperator centersOverlapOperator matrixCombiner phraseMap inputMatrices =
  let possiblePairs = liftA2 (,) inputMatrices inputMatrices
      answers = filterPairs getEdgeOfMatrix matrixSlicingOperator centersOverlapOperator possiblePairs phraseMap
      final = map matrixCombiner answers
  in nub final

cornersDoNotMatch :: MatrixPair -> Bool
cornersDoNotMatch (first, second) = getBottomLeft first /= getTopRight second

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

filterFoldable getEdgeOfMatrix matrixSlicingOperator phraseMap (first, second) =
  let froms = matrixSlicingOperator first
      possibilities = map (nextWords phraseMap) froms
      correspondences = zip (getEdgeOfMatrix second) possibilities
      answers = map wordInList correspondences
  in and answers

filterPairs getEdgeOfMatrix matrixSlicingOperator centersOverlapOperator matrixPairs phraseMap =
  filter (filterFoldable getEdgeOfMatrix matrixSlicingOperator phraseMap) candidates
  where candidates = filter (filterCandidates centersOverlapOperator) matrixPairs

combineMatrixPairHorizontally :: MatrixPair -> Matrix
combineMatrixPairHorizontally (first, second) = first M.<|> getRightColumn second

combineMatrixPairVertically :: MatrixPair -> Matrix
combineMatrixPairVertically (first, second) = first M.<-> getBottomRow second

centersOverlapVertically :: MatrixPair -> Bool
centersOverlapVertically (top, bottom) = removeTopRow top == removeBottomRow bottom

centersOverlapHorizontally :: MatrixPair -> Bool
centersOverlapHorizontally (left, right) = removeLeftColumn left == removeRightColumn right

filterCandidates :: (MatrixPair -> Bool) -> MatrixPair -> Bool
filterCandidates centersOverlapOperator pair = cornersDoNotMatch pair && centersOverlapOperator pair
