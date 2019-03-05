module Main where

import Data.List
import Control.Monad
import FoldSquare
import qualified Data.Matrix as M
import CombineHorizontal
import CombineVertical
import MapBuilder

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
      uniqueWords = nub wordList
      answer2 = foldSquare wordList uniqueWords
      phraseMap = buildPhraseMap wordList 2
      answer32 = combineHorizontal phraseMap answer2
      answer33 = combineVertical phraseMap answer32
  putStrLn "2x2 results:\n\n"
  putStr $ show answer2
  putStrLn "3x2 results:\n\n"
  putStr $ show answer32
  putStrLn "3x3 results:\n\n"
  putStr $ show answer33
