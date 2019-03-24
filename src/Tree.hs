module Tree (
            Orthotope (Point, Line, Orthotope),
            getTopRightCorner) where
import Data.Sequence

data Orthotope = Point String | Line (Seq Orthotope) | Orthotope (Seq Orthotope) deriving (Show)

getTopRightCorner :: Orthotope -> String
getTopRightCorner (Point w) = w
getTopRightCorner (Line s) = getTopRightCorner $ getLastElement $ viewr s

getLastElement :: ViewR Orthotope -> Orthotope
getLastElement (xs :> x) = x
