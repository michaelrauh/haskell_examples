module CombineNextDimension (combineNextDimension, Box (Square, Hyper)) where
import qualified Data.Matrix as M
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import Control.Applicative
import MapBuilder
import MatrixUtils

type Matrix = M.Matrix String
data Box = Square Matrix | Hyper Box Box deriving (Show, Eq)
type MapToSet = Map.Map String (S.Set String)

combineNextDimension :: MapToSet -> [Box] -> [Box]
combineNextDimension wordMap boxList =
  let allPairs = liftA2 (,) boxList boxList
      filtered = filterPairs wordMap allPairs
      combined = map combinePairs filtered
  in combined

combinePairs :: (Box, Box) -> Box
combinePairs (m1, m2) = Hyper m1 m2

filterPairs :: MapToSet -> [(Box, Box)] -> [(Box, Box)]
filterPairs wordMap = filter (filterBox wordMap)

filterBox :: MapToSet -> (Box, Box) -> Bool
filterBox wordMap (b1, b2) =
  let mappingKey = unroll b1
      potentialRightHandSide = map (nextWords wordMap) mappingKey
      correspondences = zip (unroll b2) potentialRightHandSide
      matchRecords = map wordInList correspondences
  in and matchRecords

wordInList :: (String, [String]) -> Bool
wordInList (target, possibilities) = target `elem` possibilities

unroll :: Box -> [String]
unroll (Square b) = unrollMatrix b
unroll (Hyper b1 b2) = unroll b1 ++ unroll b2
