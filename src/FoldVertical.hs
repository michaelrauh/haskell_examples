module FoldVertical
    ( executeVertical
    ) where

import Common
import Data.List
import qualified Data.Matrix as M
import MatrixTools
import Control.Monad

type MatrixPair = (Matrix, Matrix)
type Matrix = M.Matrix String

executeVertical :: [String] -> [Matrix] -> [Matrix]
executeVertical wordList inputMatrices =
  let height = M.nrows $ head inputMatrices
      phraseMap = buildPhraseMap wordList height
      possiblePairs = liftM2 (,) inputMatrices inputMatrices
      answers = filterPairs possiblePairs phraseMap
      final = map combineMatrixPair answers
  in nub final

filterCandidates :: MatrixPair -> Bool
filterCandidates (top, bottom) =
  let overlapTop = removeTopRow top
      overlapBottom = removeBottomRow bottom
      topRight = getTopRight top
      bottomLeft = getBottomLeft bottom
  in topRight /= bottomLeft && overlapTop == overlapBottom

filterFoldable phraseMap (top, bottom) =
  let nextWords' = nextWords phraseMap
      froms = getColumns top
      possibilities = mapM nextWords' froms
      targetWords = getBottomRowList bottom
      correspondences = zip targetWords possibilities
      answers = map wordInList correspondences
  in (length answers == M.nrows top) && and answers

filterPairs matrixPairs phraseMap =
  let candidates = filter filterCandidates matrixPairs
  in  filter (filterFoldable phraseMap) candidates

combineMatrixPair :: MatrixPair -> Matrix
combineMatrixPair (top, bottom) = top M.<-> getBottomRow bottom
