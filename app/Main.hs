module Main where

import Common

import Data.List
import Control.Monad
import Fold2
import Fold3

type Answer = (String, String, String, String, String)
type FormattedAnswer = (String, String, String, String)
type SetTwo = (FormattedAnswer, FormattedAnswer, FormattedAnswer, FormattedAnswer)
type Row = (String, String, String)
type Extras = (String, String, String, String, String, String)
type Answer3 = (Row, Row, Row, Extras)
type FormattedAnswer3 = (Row, Row, Row)

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
  let answer2 = execute2 wordList
  putStrLn "2x2 results:"
  putStr $ produceResult answer2
  putStrLn "3x3 results:"
  putStr $ concat $ execute3 wordList answer2

execute3 :: [String] -> [FormattedAnswer] -> [String]
execute3 wordList formattedAnswers =
  let uniqueWords = nub wordList
      possibilities = makePossibilitiesPool formattedAnswers
      candidates = nub $ filterCandidates possibilities
      foldOnWordlist = fold3 wordList
      folded = concatMap foldOnWordlist candidates
      filtered = filter filterFolded3 folded
      final = nub $ map formFinal3 filtered
  in map prettyPrint3 final
