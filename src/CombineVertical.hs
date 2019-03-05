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

type StringMatrix = M.Matrix String

type MatrixPair = (Matrix, Matrix)
type Matrix = M.Matrix String

combineVertical phraseMap inputMatrices =
  let possiblePairs = findPossiblePairs inputMatrices
      answers = filterPairs possiblePairs phraseMap
      final = map combineMatrixPair answers
  in nub final

findPossiblePairs inputMatrices = liftM2 (,) inputMatrices inputMatrices

filterCandidates :: MatrixPair -> Bool
filterCandidates pair =
  cornersDoNotMatch pair && centersOverlap pair

cornersDoNotMatch :: MatrixPair -> Bool
cornersDoNotMatch (top, bottom) =
  getBottomLeft bottom /= getTopRight top

centersOverlap (top, bottom) =
  removeTopRow top == removeBottomRow bottom

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

filterFoldable phraseMap (top, bottom) =
  let nextWords' = nextWords phraseMap
      froms = getFroms top
      possibilities = getPossibilities froms phraseMap
      correspondences = getZips possibilities bottom
      answers = getAnswers correspondences
  in checkAnswers answers

getFroms = getColumns
getPossibilities m phraseMap = map (nextWords phraseMap) m
getZips possibilities bottom = zip (getBottomRowList bottom) possibilities
getAnswers = map wordInList
checkAnswers = and

filterPairs matrixPairs phraseMap =
  let candidates = filter filterCandidates matrixPairs
  in  filter (filterFoldable phraseMap) candidates

combineMatrixPair :: MatrixPair -> Matrix
combineMatrixPair (top, bottom) = top M.<-> getBottomRow bottom
