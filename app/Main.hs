module Main where

import Common

import Data.List
import Control.Monad
import Fold2
import Fold3
import Fold4
import qualified Data.Matrix as M

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
  let uniqueWords = nub wordList
  let answer2 = execute2 wordList uniqueWords
  putStrLn "2x2 results:"
  putStr $ show answer2
