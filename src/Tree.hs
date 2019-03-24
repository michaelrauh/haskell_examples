module Tree where
import Data.Sequence

data Orthotope = Word String | Sentence (Seq Orthotope) | Square (Seq Orthotope) deriving (Show)

getTopRightCorner :: Orthotope -> String
getTopRightCorner t = ""
