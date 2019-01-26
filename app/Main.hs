module Main where

import Common

import Data.List
import Control.Monad
import Fold2
import Fold3

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
  let answer2 = execute2 wordList
  putStrLn "2x2 results:"
  putStr $ produceResult answer2
  putStrLn "3x3 results:"
  putStr $ concat $ execute3 wordList answer2
