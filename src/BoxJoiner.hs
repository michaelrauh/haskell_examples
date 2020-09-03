module BoxJoiner (
            bottomLeftCorner,
            topRightCorner,
            inLines,
            nextLines,
            inColumn,
            nextColumn,
            inCenter,
            nextCenter,
            combineDiagonals
            ) where


import qualified Orthotope as O
import qualified Data.Set as S

bottomLeftCorner :: String -> String -> String
bottomLeftCorner a b = a

topRightCorner :: String -> String -> String
topRightCorner a b = b

inLines :: O.Ortho -> O.Ortho -> O.Ortho -> O.Ortho -> O.Ortho
inLines (O.Orthotope ol1) (O.Orthotope ol2) l1 = O.zipConcat (head ol1)

nextLines :: O.Ortho -> O.Ortho -> O.Ortho -> O.Ortho -> O.Ortho
nextLines o1 o2 l1 l2 = O.zipConcat o1 o2

inColumn :: O.Ortho -> O.Ortho -> O.Ortho -> O.Ortho -> O.Ortho
inColumn o1 o2 col1 col2 = col2

nextColumn :: O.Ortho -> O.Ortho -> O.Ortho -> O.Ortho -> O.Ortho
nextColumn o1 o2 col1 col2 = o2

inCenter :: O.Ortho -> O.Ortho -> O.Ortho
inCenter = O.addLength

nextCenter :: O.Ortho -> O.Ortho -> O.Ortho
nextCenter = O.upDimension

combineDiagonals :: [S.Set String] -> [S.Set String] -> [S.Set String]
combineDiagonals f s = [head f] ++ zipWith S.union (tail f) (init s) ++ [last s]
