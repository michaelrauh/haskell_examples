module Common
    ( adjacentWords,
    adjacentFromPhrase,
    buildMap
    ) where

import Data.List
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map.Strict as Map

adjacentWords :: Int -> [String] -> String -> [String]
adjacentWords offset wordList word =
  let indices = elemIndices word wordList
      offsetIndices = map (+ offset) indices
      remaining = filter (liftM2 (&&) (> 0) (< length wordList)) offsetIndices
      answer = map (wordList !!) remaining
  in answer

adjacentFromPhrase :: Int -> [String] -> [String] -> [String]
adjacentFromPhrase offset wordList phrase =
    let allPossibilities = windows (length phrase) wordList
        indices = elemIndices phrase allPossibilities
        offsetIndices = map (+ offset) indices
        remaining = filter (liftM2 (&&) (> 0) (< length wordList)) offsetIndices
    in map (last . (allPossibilities !!)) remaining

windows :: Int -> [a] -> [[a]]
windows size = foldr (zipWith (:)) (repeat []) . take size . tails

buildSlidingTuple :: [a] -> [(a, S.Set a)]
buildSlidingTuple [] = []
buildSlidingTuple [first] = []
buildSlidingTuple [first, second] = [(first, S.singleton second)]
buildSlidingTuple (first:second:rest) = (first, S.singleton second) : buildSlidingTuple (second : rest)

buildMap :: Ord a => [a] -> Map.Map a (S.Set a)
buildMap wordList = Map.fromListWith S.union $ buildSlidingTuple wordList
