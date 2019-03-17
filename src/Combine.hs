module Combine (combineHorizontal, combineVertical) where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import MapBuilder
import MatrixUtils
import Control.Applicative
import Orthotope

type MatrixPair = (Matrix, Matrix)
type Matrix = M.Matrix String
type PhraseMap = Map.Map [String] (S.Set String)
type BoxPair = (Box, Box)

combineHorizontal :: PhraseMap -> [Box] -> [Box]
combineHorizontal = combine getRightColumnList getRows centersOverlapHorizontally combineMatrixPairHorizontally

combineVertical :: PhraseMap -> [Box] -> [Box]
combineVertical = combine getBottomRowList getColumns centersOverlapVertically combineMatrixPairVertically

combine :: (Box -> [String]) -> (Box -> [[String]]) -> (BoxPair -> Bool) -> (BoxPair -> Box) -> PhraseMap -> [Box] -> [Box]
combine getEdgeOfMatrix matrixSlicingOperator centersOverlapOperator matrixCombiner phraseMap inputMatrices =
  let possiblePairs = liftA2 (,) inputMatrices inputMatrices
      validCombinations = filterPairs getEdgeOfMatrix matrixSlicingOperator centersOverlapOperator possiblePairs phraseMap
  in nub $ map matrixCombiner validCombinations

cornersDoNotMatch :: MatrixPair -> Bool
cornersDoNotMatch (first, second) = getBottomLeft first /= getTopRight second

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

filterFoldable :: (Box -> [String]) -> (Box -> [[String]]) -> PhraseMap -> BoxPair -> Bool
filterFoldable getEdgeOfMatrix matrixSlicingOperator phraseMap (first, second) =
  let mappingKey = matrixSlicingOperator first
      potentialRightHandSide = map (nextWords phraseMap) mappingKey
      correspondences = zip (getEdgeOfMatrix second) potentialRightHandSide
      matchRecords = map wordInList correspondences
  in and matchRecords

filterPairs  :: (Box -> [String]) -> (Box -> [[String]]) -> (BoxPair -> Bool) -> [BoxPair] -> PhraseMap -> [BoxPair]
filterPairs getEdgeOfMatrix matrixSlicingOperator centersOverlapOperator matrixPairs phraseMap =
  filter (filterFoldable getEdgeOfMatrix matrixSlicingOperator phraseMap) candidates
  where candidates = filter (filterCandidates centersOverlapOperator) matrixPairs

combineMatrixPairHorizontally :: BoxPair -> Box
combineMatrixPairHorizontally p = Square $ M.fromList  1 1 ["1"]
-- combineMatrixPairHorizontally (first, second) = first M.<|> getRightColumn second

combineMatrixPairVertically :: BoxPair -> Box
combineMatrixPairVertically p = Square $ M.fromList  1 1 ["1"]
-- combineMatrixPairVertically (first, second) = first M.<-> getBottomRow second

centersOverlapVertically :: BoxPair -> Bool
centersOverlapVertically p = False
-- centersOverlapVertically (top, bottom) = removeTopRow top == removeBottomRow bottom

centersOverlapHorizontally :: BoxPair -> Bool
centersOverlapHorizontally p = False
-- centersOverlapHorizontally (left, right) = removeLeftColumn left == removeRightColumn right

filterCandidates :: (BoxPair -> Bool) -> BoxPair -> Bool
filterCandidates op p = False
-- filterCandidates centersOverlapOperator pair = cornersDoNotMatch pair && centersOverlapOperator pair
