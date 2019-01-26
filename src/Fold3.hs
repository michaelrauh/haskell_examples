module Fold3
    ( makePossibilitiesPool,
    filterCandidates,
    formFinal3,
    prettyPrint3,
    prettyPrintRow,
    candidate,
    fold3,
    filterFolded3
    ) where

import Common

type Answer = (String, String, String, String, String)
type FormattedAnswer = (String, String, String, String)
type SetTwo = (FormattedAnswer, FormattedAnswer, FormattedAnswer, FormattedAnswer)
type Row = (String, String, String)
type Extras = (String, String, String, String, String, String)
type Answer3 = (Row, Row, Row, Extras)
type FormattedAnswer3 = (Row, Row, Row)

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
