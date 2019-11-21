module Orthotope (Orthotope (Point, Orthotope), upDimension, addLength, Ortho, getNext, zipWithOrtho, zipConcat, WordMap, toList) where

import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import MapBuilder

data Orthotope a = Point a | Orthotope [Orthotope a] deriving (Eq)

type Ortho = Orthotope String
type WordMap = Map.Map String (S.Set String)

zipConcat :: Ortho -> Ortho -> Ortho
zipConcat = zipWithOrtho (++)

getNext :: AdjacentMap -> Ortho -> [Ortho]
getNext = mapM . nextWords

upDimension :: Ortho -> Ortho -> Ortho
upDimension a b = Orthotope [a, b]

addLength :: Ortho -> Ortho -> Ortho
addLength (Orthotope l1) (Orthotope l2) = Orthotope (head l1 : l2)

zipWithOrtho :: (a -> b -> c) -> Orthotope a -> Orthotope b -> Orthotope c
zipWithOrtho f (Point a) (Point b) = Point (f a b)
zipWithOrtho f (Orthotope l1) (Orthotope l2) = Orthotope (zipWith (zipWithOrtho f) l1 l2)

toList :: Orthotope a -> [a]
toList (Point a) = [a]
toList (Orthotope l) = concatMap toList l

instance Functor Orthotope where
  fmap f (Point a) = Point (f a)
  fmap f (Orthotope l) = Orthotope (map (fmap f) l)

instance Foldable Orthotope where
  foldMap f (Point a) = f a
  foldMap f (Orthotope l) = foldMap (foldMap f) l

instance Traversable Orthotope where
  traverse f (Point a) = Point <$> f a
  traverse f (Orthotope l) = Orthotope <$> traverse (traverse f) l

instance (Show a) => Show (Orthotope a) where
  show (Point a) = show a
  show (Orthotope l) = show l
