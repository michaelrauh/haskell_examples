module FoldHorizontal
    ( executeHorizontal
    ) where

import Common
import Data.List
import qualified Data.Matrix as M
import MatrixTools
import Control.Monad

type MatrixPair = (Matrix, Matrix)
type Matrix = M.Matrix String

executeHorizontal :: [String] -> [Matrix] -> [Matrix]
executeHorizontal wordList inputMatrices =
  let possiblePairs = liftM2 (,) inputMatrices inputMatrices
      answers = filterPairs possiblePairs wordList
      final = map combineMatrixPair answers
  in nub final

filterPairs :: [MatrixPair] -> [String] -> [MatrixPair]
filterPairs matrixPairs wordList =
  let candidates = filter filterCandidates matrixPairs
  in  filter (filterFoldable wordList) candidates

filterCandidates :: MatrixPair -> Bool
filterCandidates (left, right) =
  let overlapLeft = removeLeftColumn left
      overlapRight = removeRightColumn right
      topRight = getTopRight right
      bottomLeft = getBottomLeft left
  in topRight /= bottomLeft && overlapLeft == overlapRight

filterFoldable :: [String] -> MatrixPair -> Bool
filterFoldable wordList (left, right) =
  let nextWords = adjacentWords (M.ncols left) wordList
      froms = getLeftColumnList left
      possibilities = mapM nextWords froms
      targetWords = getRightColumnList right
      correspondences = zip targetWords possibilities
      answers = map wordInList correspondences
  in and answers

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

combineMatrixPair :: MatrixPair -> Matrix
combineMatrixPair (left, right) = left M.<|> getRightColumn right
