module BoxData (Box (Box, getColumn)) where
import Orthotope

data Box = Box { getOrthotope :: Orthotope String,
                 getBottomLeftCorner :: String,
                 getTopRightCorner :: String,
                 getLines :: Orthotope String,
                 getColumn :: Orthotope String
                } deriving (Show, Eq)
