module Main where

import Common

import Data.List
import Control.Monad
import Fold2
import qualified Data.Matrix as M
import FoldHorizontal

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
  let uniqueWords = rmdups wordList
  let answer2 = execute2 wordList uniqueWords
  let answer32 = executeHorizontal wordList answer2
  let answer42 = executeHorizontal wordList answer32
  putStrLn "2x2 results:"
  putStr $ show answer2
  putStrLn "3x2 results:"
  putStr $ show answer32
  putStrLn "4x2 results:"
  putStr $ show answer42

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
