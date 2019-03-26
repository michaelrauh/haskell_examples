module Tree (
            Orthotope (Point, Orthotope),
            Box (Box),
            upDimension,
            fromString,
            upBoxDimension) where

data Orthotope = Point String | Orthotope [Orthotope] deriving (Show, Eq)
data Box = Box { getOrthotope :: Orthotope,
                 getTopRightCorner :: String
                } deriving (Show, Eq)

upDimension :: Orthotope -> Orthotope -> Orthotope
upDimension a b = Orthotope [a, b]

upBoxDimension :: Box -> Box -> Box
upBoxDimension (Box o1 tr1) (Box o2 tr2) = Box (upDimension o1 o2) tr2

fromString :: String -> Box
fromString s = Box (Point s) s
