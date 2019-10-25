module Search (buildFirstBoxes) where

import MapBuilder
import Box
import BoxData

buildFirstBoxes :: String -> [Box] -- todo filter shiftedwords to remove repeated words
buildFirstBoxes corpus =
  let wordList = words corpus
      shiftedWords = drop 1 wordList
      stringTuples = zip wordList shiftedWords
      initialBoxes = map fromStringPair stringTuples
      in initialBoxes
