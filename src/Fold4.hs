module Fold4
    ( execute4,
    ) where

import Common
import Data.List
import Control.Monad

type Row = (String, String, String)
type FormattedAnswer4 = (Row, Row, Row)
type FormattedAnswer3 = (Row, Row)

execute4 :: [String] -> [FormattedAnswer3] -> [FormattedAnswer4]
execute4 wordList formattedAnswers = [(("", "", ""),("", "", ""),("", "", ""))]
