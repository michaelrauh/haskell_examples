module Main where

import Common

import Data.List
import Control.Monad
import Fold2
import Fold3
import Fold4

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
  let answer2 = execute2 wordList
  let answer3 = execute3 wordList answer2
  let answer4 = execute4 wordList answer3
  putStrLn "2x2 results:"
  putStr $ produceResult answer2
  putStrLn "3x2 results:"
  putStr $ produceResult3 answer3
  putStrLn "3x3 results:"
  putStr $ produceResult4 answer4
