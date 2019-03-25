module Tree (
            Orthotope (Point, Orthotope),
            Box(Box),
            upDimension,
            fromString) where

data Orthotope = Point String | Orthotope [Orthotope] deriving (Show, Eq)
data Box = Box { getOrthotope :: Orthotope,
                 getTopRightCorner :: String
                } deriving (Show, Eq)

upDimension :: Orthotope -> Orthotope -> Orthotope
upDimension a b = Orthotope [a, b]

upBoxDimension :: Box -> Box -> Box
upBoxDimension a b = Box (Point "foo") "bar"

fromString :: String -> Box
fromString s = Box (Point s) s
