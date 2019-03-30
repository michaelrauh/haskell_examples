module Box (
            fromStringPair,
            upDimension) where

import qualified Orthotope as O
import BoxData

upDimension :: Box -> Box -> Box
upDimension (Box o1 bl1 tr1 rc1 l1) (Box o2 bl2 tr2 rc2 l2) = Box (O.upDimension o1 o2) bl1 tr2 (O.upDimension rc1 rc2) (O.upDimension l1 l2)

fromStringPair :: (String, String) -> Box
fromStringPair (f, s) = Box (O.Orthotope [O.Point f, O.Point s]) f s (O.Point s) (O.Point (f ++ s))
