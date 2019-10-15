module Box (
            fromStringPair,
            combineBoxesNext,
            combineBoxesIn,
            getCenter1,
            getCenter2,
            eligibleNext,
            eligibleIn,
            getPossibleNext,
            getPossibleNextBoxes,
            eligibleToCombineNext,
            eligibleToCombineIn,
            getNextEligibleBoxes,
            combine,
            combineAll) where

import           BoxData
import qualified BoxJoiner           as B
import           Control.Applicative
import qualified Orthotope           as O
import MapBuilder


combineAll :: AdjacentMap -> [Box] -> [Box]
combineAll adjacentMap allBoxes = concatMap (combine adjacentMap allBoxes) allBoxes

combine :: AdjacentMap -> [Box] -> Box -> [Box]
combine wm@(Word wordMap) allBoxes box = map (combineBoxesNext box) (getNextEligibleBoxes wm allBoxes box)
combine wm@(Phrase wordMap) allBoxes box = map (combineBoxesIn box) (getNextEligibleBoxes wm allBoxes box)

getNextEligibleBoxes :: AdjacentMap -> [Box] -> Box -> [Box]
getNextEligibleBoxes wm@(Word wordMap) allBoxes box = eligibleToCombineNext (getPossibleNextBoxes wm allBoxes box) box
getNextEligibleBoxes wm@(Phrase wordMap) allBoxes box = eligibleToCombineIn (getPossibleNextBoxes wm allBoxes box) box

eligibleToCombineNext :: [Box] -> Box -> [Box]
eligibleToCombineNext nextBoxes box = filter (eligibleNext box) nextBoxes

eligibleToCombineIn :: [Box] -> Box -> [Box]
eligibleToCombineIn nextBoxes box = filter (eligibleIn box) nextBoxes

getPossibleNextBoxes :: AdjacentMap -> [Box] -> Box -> [Box]
getPossibleNextBoxes adjacentMap allBoxes box = filter (filterFunction adjacentMap box) allBoxes

filterFunction :: AdjacentMap -> Box -> Box -> Bool
filterFunction am@(Word wordMap) n x = getOrthotope x `elem` getPossibleNext am n
filterFunction am@(Phrase wordMap) n x = getColumn x `elem` getPossibleNext am n

eligibleNext :: Box -> Box -> Bool
eligibleNext b1 b2 = getBottomLeftCorner b1 /= getTopRightCorner b2

eligibleIn :: Box -> Box -> Bool
eligibleIn b1 b2 = getBottomLeftCorner b1 /= getTopRightCorner b2 && (getCenter1 b1 == getCenter2 b2)

getPossibleNext :: AdjacentMap -> Box -> [O.Ortho]
getPossibleNext am@(Word wordMap) b = O.getNext am (getOrthotope b)
getPossibleNext am@(Phrase wordMap) b = O.getNext am (getLines b)

combineBoxesNext :: Box -> Box -> Box
combineBoxesNext (Box o1 bl1 tr1 l1 c1 cen11 cen12) (Box o2 bl2 tr2 l2 c2 cen21 cen22) =
  Box (O.upDimension o1 o2)
  (B.bottomLeftCorner bl1 bl2)
  (B.topRightCorner tr1 tr2)
  (B.nextLines o1 o2 l1 l2)
  (B.nextColumn o1 o2 c1 c2)
  (B.nextCenter cen11 cen21)
  (B.nextCenter cen12 cen22)

combineBoxesIn :: Box -> Box -> Box
combineBoxesIn (Box o1 bl1 tr1 l1 c1 cen11 cen12) (Box o2 bl2 tr2 l2 c2 cen21 cen22) =
  Box (O.addLength o1 o2)
  (B.bottomLeftCorner bl1 bl2)
  (B.topRightCorner tr1 tr2)
  (B.inLines o1 o2 l1 l2)
  (B.inColumn o1 o2 c1 c2)
  (B.inCenter cen11 cen21)
  (B.inCenter cen12 cen22)

fromStringPair :: (String, String) -> Box
fromStringPair (f, s) =
  Box (O.Orthotope [O.Point f, O.Point s])
  f
  s
  (O.Point (f ++ s))
  (O.Point s)
  (O.Orthotope [O.Point s])
  (O.Orthotope [O.Point f])
