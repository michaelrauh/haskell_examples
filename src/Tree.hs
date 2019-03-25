module Tree (
            Orthotope (Point, Orthotope),
            upDimension) where

data Orthotope = Point String | Orthotope [Orthotope] deriving (Show, Eq)
data Box = Box { getOrthotope :: Orthotope,
                 getTopRightCorner :: String
                }

upDimension :: Orthotope -> Orthotope -> Orthotope
upDimension a b = Orthotope [a, b]
