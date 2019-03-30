module Box (
            Box (Box),
            fromStringPair,
            upDimension) where

import qualified Orthotope as O
data Box = Box { getOrthotope :: O.Orthotope,
                 getBottomLeftCorner :: String,
                 getTopRightCorner :: String,
                 getRightColumn :: O.Orthotope
                } deriving (Show, Eq)


upDimension :: Box -> Box -> Box
upDimension (Box o1 bl1 tr1 rc1) (Box o2 bl2 tr2 rc2) = Box (O.upDimension o1 o2) bl1 tr2 (O.findRightCol rc1 rc2)

fromStringPair :: (String, String) -> Box
fromStringPair (f, s) = Box (O.Orthotope [O.Point f, O.Point s]) f s (O.Point s)
