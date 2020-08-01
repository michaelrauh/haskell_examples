module Search (buildFirstBoxes, getAllNextDimension) where

import MapBuilder
import Box
import BoxData

buildFirstBoxes :: [String] -> [Box]
buildFirstBoxes wordList =
  let shiftedWords = drop 1 wordList
      stringTuples = zip wordList shiftedWords
      safeStringtuples = filter (uncurry (/=)) stringTuples
      initialBoxes = map fromStringPair safeStringtuples
      in initialBoxes

getAllNextDimension :: [String] -> [Box] -> [Box]
getAllNextDimension wordList boxes =
  let wordMap = buildNextWordMap wordList
      in combineAll wordMap boxes
