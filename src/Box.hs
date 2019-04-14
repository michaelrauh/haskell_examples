module Box (
            fromStringPair,
            upDimension,
            addLength) where

import qualified Orthotope as O
import BoxData

upDimension :: Box -> Box -> Box
upDimension (Box o1 bl1 tr1 l1) (Box o2 bl2 tr2 l2) =
  Box (O.upDimension o1 o2) bl1 tr2 (O.zipConcat o1 o2)

addLength :: Box -> Box -> Box
addLength (Box o1@(O.Orthotope ol1) bl1 tr1 l1) (Box o2 bl2 tr2 l2) =
  Box (O.addLength o1 o2) bl1 tr2 (O.zipConcat (head ol1) l2)

fromStringPair :: (String, String) -> Box
fromStringPair (f, s) =
  Box (O.Orthotope [O.Point f, O.Point s]) f s (O.Point (f ++ s))
