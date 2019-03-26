module Tree (
            Orthotope (Point, Orthotope),
            Box (Box),
            upDimension,
            fromString,
            upBoxDimension) where

data Orthotope = Point String | Orthotope [Orthotope] deriving (Show, Eq)
data Box = Box { getOrthotope :: Orthotope,
                 getBottomLeftCorner :: String,
                 getTopRightCorner :: String
                } deriving (Show, Eq)

upDimension :: Orthotope -> Orthotope -> Orthotope
upDimension a b = Orthotope [a, b]

upBoxDimension :: Box -> Box -> Box
upBoxDimension (Box o1 bl1 tr1) (Box o2 bl2 tr2) = Box (upDimension o1 o2) bl1 tr2

fromString :: String -> Box
fromString s = Box (Point s) s s
