module Main where

import Common

import Data.List
import Control.Monad
import Fold2
import qualified Data.Matrix as M
import FoldHorizontal
import FoldVertical

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
      uniqueWords = rmdups wordList
      answer2 = execute2 wordList uniqueWords
      answer32 = executeHorizontal wordList answer2
      answer33 = executeVertical wordList answer32
  putStrLn "2x2 results:\n\n"
  putStr $ show answer2
  putStrLn "3x2 results:\n\n"
  putStr $ show answer32
  putStrLn "3x3 results:\n\n"
  putStr $ show answer33
