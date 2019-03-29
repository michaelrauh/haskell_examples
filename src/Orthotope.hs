module Orthotope (Orthotope (Point, Orthotope), upDimension, findRightCol) where

data Orthotope = Point String | Orthotope [Orthotope] deriving (Show, Eq)

upDimension :: Orthotope -> Orthotope -> Orthotope
upDimension a b = Orthotope [a, b]

findRightCol :: Orthotope -> Orthotope -> Orthotope
findRightCol (Point s1) p@(Point s2) = p
findRightCol o1@(Orthotope l1) o2@(Orthotope l2) = Orthotope [o1, o2]
