module Box (
            fromStringPair,
            combineBoxes,
            getCenter1,
            getCenter2,
            cornersDoNotMatch,
            getPossibleNext,
            getPossibleNextBoxes,
            eligibleToCombine,
            getNextEligibleBoxes,
            combine,
            combineAll,
            Combinable (Next, In)) where

import qualified Orthotope as O
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
eligibleToCombine nextBoxes box = filter (cornersDoNotMatch box) nextBoxes;

getPossibleNextBoxes :: O.WordMap -> [Combinable] -> Combinable -> [Combinable]
getPossibleNextBoxes wordMap allBoxes box = filter (filterFunction wordMap box) allBoxes

filterFunction :: O.WordMap -> Combinable -> Combinable -> Bool
filterFunction wordMap n@(Next box) (Next x) = getOrthotope x `elem` getPossibleNext wordMap n

cornersDoNotMatch :: Combinable -> Combinable -> Bool
cornersDoNotMatch (Next b1) (Next b2) = getBottomLeftCorner b1 /= getTopRightCorner b2

getPossibleNext :: O.WordMap -> Combinable -> [O.Ortho]
getPossibleNext wordMap (Next b) = O.getNext wordMap (getOrthotope b)

getCenter1 :: Box -> O.Ortho
getCenter1 = getColumn

getCenter2 :: Box -> O.Ortho
getCenter2 (Box (O.Orthotope ol) _ _ _ _) = head ol

combineBoxes :: Combinable -> Combinable -> Box
combineBoxes (Next (Box o1 bl1 tr1 l1 c1)) (Next (Box o2 bl2 tr2 l2 c2)) = Box (O.upDimension o1 o2) bl1 tr2 (O.zipConcat o1 o2) o2

fromStringPair :: (String, String) -> Box
fromStringPair (f, s) = Box (O.Orthotope [O.Point f, O.Point s]) f s (O.Point (f ++ s)) (O.Point s)
