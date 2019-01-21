module Main where

import Lib

import Data.List
import Control.Monad

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
  putStr $ concat (execute contents)

execute :: String -> [String]
execute input =
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

adjacentWords :: Int -> [String] -> String -> [String]
adjacentWords offset wordList word =
      let indices = elemIndices word wordList
          offsetIndices = map (+ offset) indices
          remaining = filter (liftM2 (&&) (> 0) (< length wordList)) offsetIndices
  in map (wordList !!) remaining

foldWord :: [String] -> String -> [Answer]
foldWord wordList a =
  let nextWords = adjacentWords 1 wordList
      prevWords = adjacentWords (-1) wordList
  in
  do
      b <- nextWords a
      d <- nextWords b
      c <- prevWords d
      a' <- prevWords c
      return (a, b, d, c, a')

makePossibilitiesPool :: [FormattedAnswer] -> [SetTwo]
makePossibilitiesPool answer =
  do
    tL <- answer
    tR <- answer
    bL <- answer
    bR <- answer
    return (tL, tR, bL, bR)

filterCandidates :: [SetTwo] -> [SetTwo]
filterCandidates = filter candidate

filterFoldedWords :: [Answer] -> [Answer]
filterFoldedWords = filter(\(a, b, _, c, a') -> b /= c && a == a')

formFinalAnswer :: Answer -> FormattedAnswer
formFinalAnswer (a, b, d, c, _) = (a, b, d, c)

prettyPrint :: FormattedAnswer -> String
prettyPrint (a, b, d, c) = a  ++ " " ++ b ++ "\n" ++ c ++ " " ++ d ++ "\n\n"

formFinal3 :: Answer3 -> FormattedAnswer3
formFinal3 (t, m, b, _) = (t, m, b)

prettyPrint3 :: FormattedAnswer3 -> String
prettyPrint3 (t, m, b) = prettyPrintRow t ++ prettyPrintRow m ++ prettyPrintRow b ++ "\n"

prettyPrintRow :: Row -> String
prettyPrintRow (a, b, c) = a  ++ " " ++ b ++ " " ++ c ++ "\n"

candidate :: SetTwo -> Bool
candidate ((a, b, e, d), (b', c, f, e'), (d', e'', h, g), (e''', f', i, h')) =
  c /= g && b == b' && d == d' && e == e' && e == e'' && e == e''' && f == f' && h == h'

fold3 :: [String] -> SetTwo -> [Answer3]
fold3 wordList (tL, tR, bL, bR)  =
  let (a, b, e, d) = tL
      (_, c, f, _) = tR
      (_, _, h, g) = bL
      (_, _, i, _) = bR
      nextTwo = adjacentWords 2 wordList
  in
  do
    c' <- nextTwo a
    f' <- nextTwo d
    i' <- nextTwo g
    g' <- nextTwo a
    h' <- nextTwo b
    i'' <- nextTwo c
    return ((a, b, c),(d, e, f),(g, h, i),(c', f', i', g', h', i''))

filterFolded3 :: Answer3 -> Bool
filterFolded3 ((a, b, c),(d, e, f),(g, h, i),(c', f', i', g', h', i'')) =
  c' == c && f' == f && i' == i && g' == g && h' == h && i'' == i
