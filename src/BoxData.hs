module BoxData (Box (Box, getColumn, getOrthotope, getBottomLeftCorner, getTopRightCorner, getLines, getCenter1, getCenter2)) where
import Orthotope

data Box = Box { getOrthotope :: Orthotope String,
                 getBottomLeftCorner :: String,
                 getTopRightCorner :: String,
                 getLines :: Orthotope String,
                 getColumn :: Orthotope String,
                 getCenter1 :: Orthotope String,
                 getCenter2 :: Orthotope String
                } deriving (Eq)
