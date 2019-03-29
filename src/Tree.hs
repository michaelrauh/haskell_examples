module Tree (
            Orthotope (Point, Orthotope),
            Box (Box),
            upDimension,
            fromString,
            upBoxDimension) where

data Orthotope = Point String | Orthotope [Orthotope] deriving (Show, Eq)
data Box = Box { getOrthotope :: Orthotope,
                 getBottomLeftCorner :: String,
                 getTopRightCorner :: String,
                 getRightColumn :: Orthotope
                } deriving (Show, Eq)

upDimension :: Orthotope -> Orthotope -> Orthotope
upDimension a b = Orthotope [a, b]

upBoxDimension :: Box -> Box -> Box
upBoxDimension (Box o1 bl1 tr1 rc1) (Box o2 bl2 tr2 rc2) = Box (upDimension o1 o2) bl1 tr2 (findRightCol rc1 rc2)

findRightCol :: Orthotope -> Orthotope -> Orthotope
findRightCol (Point s1) p@(Point s2) = p
findRightCol o1@(Orthotope l1) o2@(Orthotope l2) = Orthotope [o1, o2]

fromString :: String -> Box
fromString s = Box (Point s) s s (Point s)
