module Fold4
    ( execute4,
    produceResult4
    ) where

import Common
import Data.List
import Control.Monad

type Row = (String, String, String)
type FormattedAnswer4 = (Row, Row, Row)
type FormattedAnswer3 = (Row, Row)
type Answer4 = (Row, Row, Row, Extras)
type Extras = (String, String, String)
type Possibility = (FormattedAnswer3, FormattedAnswer3)

execute4 :: [String] -> [FormattedAnswer3] -> [FormattedAnswer4]
execute4 wordList formattedAnswers =
  let uniqueWords = nub wordList
      possibilities = liftM2 (,) formattedAnswers formattedAnswers
      candidates = filter candidate possibilities
      foldOnWordlist = fold4 wordList
      folded = concatMap foldOnWordlist candidates
      filtered = filter filterFolded4 folded
  in nub $ map formFinal4 filtered

produceResult4 :: [FormattedAnswer4] -> String
produceResult4 = concatMap prettyPrint4

prettyPrint4 :: FormattedAnswer4 -> String
prettyPrint4 (t, m, b) = prettyPrintRow t  ++ prettyPrintRow m ++ prettyPrintRow b ++ "\n"

candidate :: Possibility -> Bool
candidate (((a, b, c), (d, e, f)), ((d', e', f'), (g, h, i))) = d == d' && e == e' && f == f'

fold4 wordList (((a, b, c), (d, e, f)), ((d', e', f'), (g, h, i))) =
  let nextTwo = adjacentWords 2 wordList
  in
  do
    g' <- nextTwo a
    h' <- nextTwo b
    i' <- nextTwo c
    return ((a, b, c), (d, e, f), (g, h, i), (g', h', i'))

filterFolded4 ((a, b, c), (d, e, f), (g, h, i), (g', h', i')) = g == g' && h == h' && i == i'

formFinal4 (t, m, b, _) = (t, m, b)
