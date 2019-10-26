module Search (buildFirstBoxes) where

import MapBuilder
import Box
import BoxData

buildFirstBoxes :: [String] -> [Box]
buildFirstBoxes wordList =
  let shiftedWords = drop 1 wordList
      stringTuples = zip wordList shiftedWords
      safeStringtuples = filter (\(a, b) -> a /= b) stringTuples
      initialBoxes = map fromStringPair safeStringtuples
      in initialBoxes
