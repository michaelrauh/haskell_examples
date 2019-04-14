module Orthotope (Orthotope (Point, Orthotope), upDimension, addLength, Ortho, getNext) where

import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import MapBuilder

data Orthotope a = Point a | Orthotope [Orthotope a] deriving (Show, Eq)

type Ortho = Orthotope String
type WordMap = Map.Map String (S.Set String)

getNext :: WordMap -> Ortho -> [Ortho]
getNext wordMap = mapM (nextWords wordMap)

upDimension :: Ortho -> Ortho -> Ortho
upDimension a b = Orthotope [a, b]

addLength :: Ortho -> Ortho -> Ortho
addLength (Orthotope l1) (Orthotope l2) = Orthotope (head l1 : l2)

instance Functor Orthotope where
  fmap f (Point a) = Point (f a)
  fmap f (Orthotope l) = Orthotope (map (fmap f) l)

instance Foldable Orthotope where
  foldMap f (Point a) = f a
  foldMap f (Orthotope l) = foldMap (foldMap f) l

instance Traversable Orthotope where
  traverse f (Point a) = Point <$> f a
  traverse f (Orthotope l) = Orthotope <$> traverse (traverse f) l
