module Fold3
    ( execute3,
    produceResult3
    ) where

import Common
import Data.List
import Control.Monad

type FormattedAnswer = (String, String, String, String)
type SetTwo = (FormattedAnswer, FormattedAnswer)
type Row = (String, String, String)
type Extras = (String, String)
type Answer3 = (Row, Row, Extras)
type FormattedAnswer3 = (Row, Row)

execute3 :: [String] -> [FormattedAnswer] -> [FormattedAnswer3]
execute3 wordList formattedAnswers =
  let uniqueWords = nub wordList
      possibilities = makePossibilitiesPool formattedAnswers
      candidates = filterCandidates possibilities
      foldOnWordlist = fold3 wordList
      folded = concatMap foldOnWordlist candidates
      filtered = filter filterFolded3 folded
  in nub $ map formFinal3 filtered

makePossibilitiesPool :: [FormattedAnswer] -> [SetTwo]
makePossibilitiesPool answer = liftM2 (,) answer answer

produceResult3 :: [FormattedAnswer3] -> String
produceResult3 = concatMap prettyPrint3

filterCandidates :: [SetTwo] -> [SetTwo]
filterCandidates = filter candidate

formFinal3 :: Answer3 -> FormattedAnswer3
formFinal3 (t, b, _) = (t, b)

prettyPrint3 :: FormattedAnswer3 -> String
prettyPrint3 (t, b) = prettyPrintRow t ++ prettyPrintRow b ++ "\n"

candidate :: SetTwo -> Bool
candidate ((a, b, e, d), (b', c, f, e')) =
  b == b' && e == e'

fold3 :: [String] -> SetTwo -> [Answer3]
fold3 wordList (l, r)  =
  let (a, b, e, d) = l
      (_, c, f, _) = r
      nextTwo = adjacentWords 2 wordList
  in
  do
    c' <- nextTwo a
    f' <- nextTwo d
    g' <- nextTwo a
    h' <- nextTwo b
    i'' <- nextTwo c
    return ((a, b, c),(d, e, f),(c', f'))

filterFolded3 :: Answer3 -> Bool
filterFolded3 ((a, b, c),(d, e, f),(c', f')) = c' == c && f' == f
