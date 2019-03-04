module CombineHorizontal
    ( combineHorizontal,
    findWidth,
    findPossiblePairs,
    cornersDoNotMatch,
    centersOverlap
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

combineHorizontal phraseMap inputMatrices =
  let width = findWidth inputMatrices
      possiblePairs = findPossiblePairs inputMatrices
      answers = filterPairs possiblePairs phraseMap
      final = map combineMatrixPair answers
  in nub final

findPossiblePairs inputMatrices = liftM2 (,) inputMatrices inputMatrices
findWidth inputMatrices = M.ncols $ head inputMatrices

filterCandidates :: MatrixPair -> Bool
filterCandidates pair =
  cornersDoNotMatch pair && centersOverlap pair

cornersDoNotMatch :: MatrixPair -> Bool
cornersDoNotMatch (left, right) =
  getBottomLeft left /= getTopRight right

centersOverlap (left, right) =
  removeLeftColumn left == removeRightColumn right

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

filterFoldable phraseMap (left, right) =
  let nextWords' = nextWords phraseMap
      froms = getRows left
      possibilities = mapM nextWords' froms
      targetWords = getRightColumnList right
      correspondences = zip targetWords possibilities
      answers = map wordInList correspondences
  in (length answers == M.ncols left) && and answers

filterPairs matrixPairs phraseMap =
  let candidates = filter filterCandidates matrixPairs
  in  filter (filterFoldable phraseMap) candidates

combineMatrixPair :: MatrixPair -> Matrix
combineMatrixPair (left, right) = left M.<|> getRightColumn right
