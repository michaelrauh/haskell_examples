module BoxData (Box (Box, getColumn, getOrthotope, getLines, getCenter1, getCenter2)) where
import Orthotope

data Box = Box { getOrthotope :: Orthotope String,
                 getLines :: Orthotope String,
                 getColumn :: Orthotope String,
                 getCenter1 :: Orthotope String,
                 getCenter2 :: Orthotope String
                } deriving (Eq)
