module Common
    (
    nextWords,
    buildPhraseMap,
    wordInList,
    rmdups
    ) where

import Data.List
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as Map

nextWords :: Ord k => Map.Map k (S.Set a) -> k -> [a]
nextWords m key = S.toList (Map.findWithDefault S.empty key m)

windows :: Int -> [a] -> [[a]]
windows size = foldr (zipWith (:)) (repeat []) . take size . tails

buildSlidingPhraseTuple :: [a] -> Int -> [([a], S.Set a)]
buildSlidingPhraseTuple wordList phraseLength
  | length wordList > phraseLength = unsafeBuildTuple wordList phraseLength
  | otherwise = []

unsafeBuildTuple :: [a] -> Int -> [([a], S.Set a)]
unsafeBuildTuple wordList phraseLength =
  (take phraseLength wordList, nextWord) : buildSlidingPhraseTuple (drop 1 wordList) phraseLength
  where nextWord = S.singleton(head $ drop phraseLength wordList)

buildPhraseMap :: Ord a => [a] -> Int -> Map.Map [a] (S.Set a)
buildPhraseMap wordList phraseLength = Map.fromListWith S.union $ buildSlidingPhraseTuple wordList phraseLength

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
