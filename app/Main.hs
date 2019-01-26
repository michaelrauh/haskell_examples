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
  let answer2 = execute2  contents
  putStr $ produceResult answer2

execute3 :: String -> [String]
execute3 input =
  let wordList = words input
      uniqueWords = nub wordList
      answers = concatMap (filterFoldedWords . foldWord wordList) uniqueWords
      formattedAnswers = map formFinalAnswer $ nub answers
      possibilities = makePossibilitiesPool formattedAnswers
      candidates = nub $ filterCandidates possibilities
      foldOnWordlist = fold3 wordList
      folded = concatMap foldOnWordlist candidates
      filtered = filter filterFolded3 folded
      final = nub $ map formFinal3 filtered
  in map prettyPrint3 final
