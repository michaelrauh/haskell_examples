module Tree (
            Orthotope (Point, Orthotope),
            upDimension) where

data Orthotope = Point String | Orthotope [Orthotope] deriving (Show, Eq)

upDimension :: Orthotope -> Orthotope -> Orthotope
upDimension (Point a) (Point b) = Orthotope [Point a, Point b]
upDimension (Orthotope a) (Orthotope b) = Orthotope [Orthotope a, Orthotope b]
