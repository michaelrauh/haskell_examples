module BoxData (Box (Box)) where
import Orthotope

data Box = Box { getOrthotope :: Orthotope,
                 getBottomLeftCorner :: String,
                 getTopRightCorner :: String,
                 getRightColumn :: Orthotope
                } deriving (Show, Eq)
