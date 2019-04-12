module Box (
            fromStringPair,
            upDimension,
            addLength) where

import qualified Orthotope as O
import BoxData

type Combiner =  O.Ortho -> O.Ortho -> O.Ortho

upDimension :: Box -> Box -> Box
upDimension = mix O.upDimension

addLength :: Box -> Box -> Box
addLength = mix O.addLength

mix :: Combiner -> Box -> Box -> Box
mix c (Box o1 bl1 tr1 rc1 l1) (Box o2 bl2 tr2 rc2 l2) = Box (c o1 o2) bl1 tr2 (c rc1 rc2) (c l1 l2)

fromStringPair :: (String, String) -> Box
fromStringPair (f, s) =
  Box (O.Orthotope [O.Point f, O.Point s]) f s (O.Point s) (O.Point (f ++ s))
