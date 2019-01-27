module Common
    ( adjacentWords,
    prettyPrintRow
    ) where

import Data.List
import Control.Monad

type Row = (String, String, String)

adjacentWords :: Int -> [String] -> String -> [String]
adjacentWords offset wordList word =
      let indices = elemIndices word wordList
          offsetIndices = map (+ offset) indices
          remaining = filter (liftM2 (&&) (> 0) (< length wordList)) offsetIndices
  in map (wordList !!) remaining

prettyPrintRow :: Row -> String
prettyPrintRow (a, b, c) = a  ++ " " ++ b ++ " " ++ c ++ "\n"
