module Orthotope (Orthotope (Point, Orthotope), upDimension) where

data Orthotope a = Point a | Orthotope [Orthotope a] deriving (Show, Eq)

type Ortho = Orthotope String

upDimension :: Ortho -> Ortho -> Ortho
upDimension a b = Orthotope [a, b]

instance Functor Orthotope where
  fmap f (Point a) = Point (f a)
