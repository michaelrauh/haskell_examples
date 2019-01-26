module Common
    ( adjacentWords
    ) where

import Data.List
import Control.Monad

adjacentWords :: Int -> [String] -> String -> [String]
adjacentWords offset wordList word =
      let indices = elemIndices word wordList
          offsetIndices = map (+ offset) indices
          remaining = filter (liftM2 (&&) (> 0) (< length wordList)) offsetIndices
  in map (wordList !!) remaining
