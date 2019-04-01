module Orthotope (Orthotope (Point, Orthotope), upDimension) where

data Orthotope a = Point a | Orthotope [Orthotope a] deriving (Show, Eq)

type Ortho = Orthotope String

upDimension :: Ortho -> Ortho -> Ortho
upDimension a b = Orthotope [a, b]

instance Functor Orthotope where
  fmap f (Point a) = Point (f a)
  fmap f (Orthotope l) = Orthotope (map (fmap f) l)

instance Foldable Orthotope where
  foldMap f (Point a) = f a
  foldMap f (Orthotope l) = foldMap (foldMap f) l
