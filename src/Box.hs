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
            combineAll) where

import qualified Orthotope as O
import BoxData
import Control.Applicative

data Combinable = NextDimension Box | Add Box

combineAll :: O.WordMap -> [Box] -> [Box]
combineAll wordMap allBoxes = concatMap (combine wordMap allBoxes) allBoxes

combine :: O.WordMap -> [Box] -> Box -> [Box]
combine wordMap allBoxes box = map (combineBoxes box) (getNextEligibleBoxes wordMap allBoxes box)

getNextEligibleBoxes :: O.WordMap -> [Box] -> Box -> [Box]
getNextEligibleBoxes wordMap allBoxes box = eligibleToCombine (getPossibleNextBoxes wordMap allBoxes box) box

eligibleToCombine :: [Box] -> Box -> [Box]
eligibleToCombine nextBoxes box = filter (cornersDoNotMatch box) nextBoxes;

getPossibleNextBoxes :: O.WordMap -> [Box] -> Box -> [Box]
getPossibleNextBoxes wordMap allBoxes box = filter (\x -> getOrthotope x `elem` getPossibleNext wordMap box) allBoxes

cornersDoNotMatch :: Box -> Box -> Bool
cornersDoNotMatch b1 b2 = getBottomLeftCorner b1 /= getTopRightCorner b2

getPossibleNext :: O.WordMap -> Box -> [O.Ortho]
getPossibleNext wordMap b = O.getNext wordMap (getOrthotope b)

getCenter1 :: Box -> O.Ortho
getCenter1 = getColumn

getCenter2 :: Box -> O.Ortho
getCenter2 (Box (O.Orthotope ol) _ _ _ _) = head ol

combineBoxes :: Box -> Box -> Box
combineBoxes (Box o1 bl1 tr1 l1 c1) (Box o2 bl2 tr2 l2 c2) = Box (O.upDimension o1 o2) bl1 tr2 (O.zipConcat o1 o2) o2

fromStringPair :: (String, String) -> Box
fromStringPair (f, s) = Box (O.Orthotope [O.Point f, O.Point s]) f s (O.Point (f ++ s)) (O.Point s)
