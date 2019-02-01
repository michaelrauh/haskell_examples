module Common
    ( adjacentWords,
    prettyPrintRow,
    adjacentFromPhrase
    ) where

import Data.List
import Control.Monad

type Row = (String, String, String)

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
        finals = map (allPossibilities !!) remaining
    in map last finals

prettyPrintRow :: Row -> String
prettyPrintRow (a, b, c) = a  ++ " " ++ b ++ " " ++ c ++ "\n"

windows :: Int -> [a] -> [[a]]
windows size = foldr (zipWith (:)) (repeat []) . take size . tails
