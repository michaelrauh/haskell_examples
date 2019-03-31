module BoxData (Box (Box)) where
import Orthotope

data Box = Box { getOrthotope :: Orthotope String,
                 getBottomLeftCorner :: String,
                 getTopRightCorner :: String,
                 getRightColumn :: Orthotope String,
                 getLines :: Orthotope String
                } deriving (Show, Eq)
