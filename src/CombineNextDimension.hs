module CombineNextDimension (combineNextDimension, Box (Square, Hyper)) where
import qualified Data.Matrix as M
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import Control.Applicative

type Matrix = M.Matrix String
data Box = Square Matrix | Hyper Box Box deriving (Show, Eq)
type PhraseMap = Map.Map [String] (S.Set String)

combineNextDimension :: PhraseMap -> [Box] -> [Box]
combineNextDimension phraseMap boxList =
  let allPairs = liftA2 (,) boxList boxList
      filtered = filterPairs allPairs
      combined = map combinePairs filtered
  in combined

combinePairs :: (Box, Box) -> Box
combinePairs (m1, m2) = m1

filterPairs :: [(Box, Box)] -> [(Box, Box)]
filterPairs boxList = boxList
