{-# LANGUAGE TupleSections #-}

module Box (
            fromStringPair,
            combineAll) where

import           BoxData
import qualified BoxJoiner           as B
import           Control.Applicative
import qualified Orthotope           as O
import MapBuilder
import qualified Data.Set as S
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

combineAll :: AdjacentMap -> [Box] -> [Box]
combineAll adjacentMap knownBoxes =
  let projectionMap = getProjectionMap adjacentMap knownBoxes
      filteredProjections = filterProjections knownBoxes projectionMap
      orthoToBox = getOrthoToBox knownBoxes
      combinationTuples = getCombinationTuples orthoToBox filteredProjections
      allBoxes = getAllBoxes adjacentMap combinationTuples
  in allBoxes

getOrthoToBox :: [Box] -> Map.Map O.Ortho Box
getOrthoToBox boxes = Map.fromList $ fmap (\box -> (getOrthotope box, box)) boxes

getProjectionMap :: AdjacentMap -> [Box] -> Map.Map O.Ortho (S.Set Box)
getProjectionMap adjacentMap knownBoxes =
  let projections = map (getPossibleNext adjacentMap) knownBoxes
      tuples = zip knownBoxes projections
      fixedTuples = fixTuples tuples
  in Map.fromListWith S.union fixedTuples

fixTuples :: [(Box, S.Set O.Ortho)] -> [(O.Ortho, S.Set Box)]
fixTuples = concatMap fixTuple 

fixTuple :: (Box, S.Set O.Ortho) -> [(O.Ortho, S.Set Box)]
fixTuple (box, orthoSet) = 
  let orthos = S.toList orthoSet
  in fmap (fixit box) orthos

fixit :: Box -> O.Ortho -> (O.Ortho, S.Set Box)
fixit box ortho = (ortho, S.singleton box)

filterProjections :: [Box] -> Map.Map O.Ortho (S.Set Box) -> Map.Map O.Ortho (S.Set Box)
filterProjections knownBoxes projectionMap = Map.restrictKeys projectionMap (S.fromList $ getOrthotope <$> knownBoxes)

getCombinationTuples :: Map.Map O.Ortho Box -> Map.Map O.Ortho (S.Set Box) -> [(Box, Box)]
getCombinationTuples orthoToBox filteredProjectionMap =
  let listy = Map.toList filteredProjectionMap
      foo = concatMap expand listy
  in fmap (lookupfy orthoToBox) foo

lookupfy :: Map.Map O.Ortho Box -> (O.Ortho, Box) -> (Box, Box)
lookupfy orthoToBox (ortho, box) = (orthoToBox Map.! ortho, box) 

expand :: (O.Ortho, S.Set Box) -> [(O.Ortho, Box)]
expand (ortho, boxSet) = 
  let boxes = S.toList boxSet 
  in fmap (ortho,) boxes

getAllBoxes :: AdjacentMap -> [(Box, Box)] -> [Box]
getAllBoxes adjacentMap = mapMaybe (combineBoxes adjacentMap)

combineBoxes :: AdjacentMap -> (Box, Box) -> Maybe Box
combineBoxes (Word _) (b2, b1) = if eligibleNext b1 b2 then Just $ combineBoxesNext b1 b2 else Nothing
combineBoxes (Phrase _ ) (b2, b1) = if eligibleIn b1 b2 then Just $ combineBoxesIn b1 b2 else Nothing

eligibleNext :: Box -> Box -> Bool
eligibleNext b1 b2 = and $ zipWith S.disjoint (tail $ getDiagonals b1) (init $ getDiagonals b2)

eligibleIn :: Box -> Box -> Bool
eligibleIn b1 b2 = getCenter1 b1 == getCenter2 b2

getPossibleNext :: AdjacentMap -> Box -> S.Set O.Ortho
getPossibleNext am@(Word wordMap) b = S.fromList $ O.getNext am (getOrthotope b)
getPossibleNext am@(Phrase wordMap) b = S.fromList $ O.getNext am (getLines b)

combineBoxesNext :: Box -> Box -> Box
combineBoxesNext (Box o1 l1 c1 cen11 cen12 diag1) (Box o2 l2 c2 cen21 cen22 diag2) =
  Box (O.upDimension o1 o2)
  (B.nextLines o1 o2 l1 l2)
  (B.nextColumn o1 o2 c1 c2)
  (B.nextCenter cen11 cen21)
  (B.nextCenter cen12 cen22)
  (B.combineDiagonals diag1 diag2)

combineBoxesIn :: Box -> Box -> Box
combineBoxesIn (Box o1 l1 c1 cen11 cen12 diag1) (Box o2 l2 c2 cen21 cen22 diag2) =
  Box (O.addLength o1 o2)
  (B.inLines o1 o2 l1 l2)
  (B.inColumn o1 o2 c1 c2)
  (B.inCenter cen11 cen21)
  (B.inCenter cen12 cen22)
  (B.combineDiagonals diag1 diag2)

fromStringPair :: (String, String) -> Box
fromStringPair (f, s) =
  Box (O.Orthotope [O.Point f, O.Point s])
  (O.Point (f ++ s))
  (O.Point s)
  (O.Orthotope [O.Point s])
  (O.Orthotope [O.Point f])
  [S.singleton f, S.singleton s]