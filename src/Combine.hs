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
type PhraseMap = Map.Map [String] (S.Set String)

combineHorizontal :: PhraseMap -> [Matrix] -> [Matrix]
combineHorizontal = combine getRightColumnList getRows centersOverlapHorizontally combineMatrixPairHorizontally

combineVertical :: PhraseMap -> [Matrix] -> [Matrix]
combineVertical = combine getBottomRowList getColumns centersOverlapVertically combineMatrixPairVertically

combine :: (Matrix -> [String]) -> (Matrix -> [[String]]) -> (MatrixPair -> Bool) -> (MatrixPair -> Matrix) -> PhraseMap -> [Matrix] -> [Matrix]
combine getEdgeOfMatrix matrixSlicingOperator centersOverlapOperator matrixCombiner phraseMap inputMatrices =
  let possiblePairs = liftA2 (,) inputMatrices inputMatrices
      validCombinations = filterPairs getEdgeOfMatrix matrixSlicingOperator centersOverlapOperator possiblePairs phraseMap
  in nub $ map matrixCombiner validCombinations

cornersDoNotMatch :: MatrixPair -> Bool
cornersDoNotMatch (first, second) = getBottomLeft first /= getTopRight second

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

filterFoldable :: (Matrix -> [String]) -> (Matrix -> [[String]]) -> PhraseMap -> MatrixPair -> Bool
filterFoldable getEdgeOfMatrix matrixSlicingOperator phraseMap (first, second) =
  let mappingKey = matrixSlicingOperator first
      potentialRightHandSide = map (nextWords phraseMap) mappingKey
      correspondences = zip (getEdgeOfMatrix second) potentialRightHandSide
      matchRecords = map wordInList correspondences
  in and matchRecords

filterPairs  :: (Matrix -> [String]) -> (Matrix -> [[String]]) -> (MatrixPair -> Bool) -> [MatrixPair] -> PhraseMap -> [MatrixPair]
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
