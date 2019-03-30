module Orthotope (Orthotope (Point, Orthotope), upDimension) where

data Orthotope = Point String | Orthotope [Orthotope] deriving (Show, Eq)

upDimension :: Orthotope -> Orthotope -> Orthotope
upDimension a b = Orthotope [a, b]
