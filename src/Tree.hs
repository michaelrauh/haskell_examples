module Tree (
            Orthotope (Point, Line, Orthotope),
            growDimension) where

data Orthotope = Point String | Line [Orthotope] | Orthotope [Orthotope] deriving (Show, Eq)

growDimension :: Orthotope -> Orthotope -> Orthotope
growDimension (Point a) (Point b) = Line [Point a, Point b]
