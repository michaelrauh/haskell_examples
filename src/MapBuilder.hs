module MapBuilder
    ( buildNextWordMap,
    buildPhraseMap,
    nextWords,
    AdjacentMap (Word, Phrase)
    ) where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
data AdjacentMap = Word (Map.Map String (S.Set String)) | Phrase (Map.Map String (S.Set String)) deriving (Show, Eq)

buildNextWordMap :: [String] -> AdjacentMap
buildNextWordMap wordList = Word $ Map.fromListWith S.union $ buildSlidingTuple wordList

buildPhraseMap :: [String] -> Int -> AdjacentMap
buildPhraseMap wordList phraseLength = Phrase $ Map.fromListWith S.union $ buildSlidingPhraseTuple wordList phraseLength

nextWords :: AdjacentMap -> String -> [String]
nextWords (Word m) key = S.toList (Map.findWithDefault S.empty key m)
nextWords (Phrase m) key = S.toList (Map.findWithDefault S.empty key m)

buildSlidingPhraseTuple :: [String] -> Int -> [(String, S.Set String)]
buildSlidingPhraseTuple wordList phraseLength
  | length wordList > phraseLength = unsafeBuildTuple wordList phraseLength
  | otherwise = []

unsafeBuildTuple :: [String] -> Int -> [(String, S.Set String)]
unsafeBuildTuple wordList phraseLength =
  (concat $ take phraseLength wordList, nextWord) : buildSlidingPhraseTuple (drop 1 wordList) phraseLength
  where nextWord = S.singleton(head $ drop phraseLength wordList)

buildSlidingTuple :: [String] -> [(String, S.Set String)]
buildSlidingTuple [] = []
buildSlidingTuple [first] = []
buildSlidingTuple [first, second] = [(first, S.singleton second)]
buildSlidingTuple (first:second:rest) = (first, S.singleton second) : buildSlidingTuple (second : rest)
