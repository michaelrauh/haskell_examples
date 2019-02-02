module Common
    (
    adjacentFromPhrase,
    ) where

import Data.List
import Control.Monad

adjacentFromPhrase :: Int -> [String] -> [String] -> [String]
adjacentFromPhrase offset wordList phrase =
    let allPossibilities = windows (length phrase) wordList
        indices = elemIndices phrase allPossibilities
        offsetIndices = map (+ offset) indices
        remaining = filter (liftM2 (&&) (> 0) (< length wordList)) offsetIndices
    in map (last . (allPossibilities !!)) remaining

windows :: Int -> [a] -> [[a]]
windows size = foldr (zipWith (:)) (repeat []) . take size . tails
