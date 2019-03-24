module Tree (
            Orthotope (Point, Line, Orthotope),
            getTopRightCorner) where
import Data.Sequence

data Orthotope = Point String | Line (Seq Orthotope) | Orthotope (Seq Orthotope) deriving (Show)

getTopRightCorner :: Orthotope -> String
getTopRightCorner (Point w) = w
