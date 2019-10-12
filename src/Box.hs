module Box (
            fromStringPair,
            combineBoxes,
            getCenter1,
            getCenter2,
            eligible,
            getPossibleNext,
            getPossibleNextBoxes,
            eligibleToCombine,
            getNextEligibleBoxes,
            combine,
            combineAll,
            Combinable (Next, In)) where

import qualified Orthotope as O
import qualified BoxJoiner as B
import BoxData
import Control.Applicative

data Combinable = Next Box | In Box deriving (Show, Eq)

combineAll :: O.WordMap -> [Combinable] -> [Box]
combineAll wordMap allBoxes = concatMap (combine wordMap allBoxes) allBoxes

combine :: O.WordMap -> [Combinable] -> Combinable -> [Box]
combine wordMap allBoxes box = map (combineBoxes box) (getNextEligibleBoxes wordMap allBoxes box)

getNextEligibleBoxes :: O.WordMap -> [Combinable] -> Combinable -> [Combinable]
getNextEligibleBoxes wordMap allBoxes box = eligibleToCombine (getPossibleNextBoxes wordMap allBoxes box) box

eligibleToCombine :: [Combinable] -> Combinable -> [Combinable]
eligibleToCombine nextBoxes box = filter (eligible box) nextBoxes;

getPossibleNextBoxes :: O.WordMap -> [Combinable] -> Combinable -> [Combinable]
getPossibleNextBoxes wordMap allBoxes box = filter (filterFunction wordMap box) allBoxes

filterFunction :: O.WordMap -> Combinable -> Combinable -> Bool
filterFunction wordMap n@(Next box) (Next x) = getOrthotope x `elem` getPossibleNext wordMap n
filterFunction wordMap n@(In box) (In x) = getColumn x `elem` getPossibleNext wordMap n

eligible :: Combinable -> Combinable -> Bool
eligible (Next b1) (Next b2) = getBottomLeftCorner b1 /= getTopRightCorner b2
eligible (In b1) (In b2) = getBottomLeftCorner b1 /= getTopRightCorner b2 && (getCenter1 b1 == getCenter2 b2)

getPossibleNext :: O.WordMap -> Combinable -> [O.Ortho]
getPossibleNext wordMap (Next b) = O.getNext wordMap (getOrthotope b)
getPossibleNext wordMap (In b) = O.getNext wordMap (getLines b)

combineBoxes :: Combinable -> Combinable -> Box
combineBoxes (Next (Box o1 bl1 tr1 l1 c1 cen11 cen12)) (Next (Box o2 bl2 tr2 l2 c2 cen21 cen22)) =
  Box (O.upDimension o1 o2)
  (B.bottomLeftCorner bl1 bl2)
  (B.topRightCorner tr1 tr2)
  (B.nextLines o1 o2 l1 l2)
  (B.nextColumn o1 o2 c1 c2)
  (B.nextCenter cen11 cen21)
  (B.nextCenter cen12 cen22)
combineBoxes (In (Box o1 bl1 tr1 l1 c1 cen11 cen12)) (In (Box o2 bl2 tr2 l2 c2 cen21 cen22)) =
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
