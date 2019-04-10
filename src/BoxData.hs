module BoxData (Box (Box)) where
import Orthotope

data Box = Box { getOrthotope :: Orthotope String,
                 getBottomLeftCorner :: String,
                 getTopRightCorner :: String,
                 getRightColumn :: Orthotope String,
                 getLines :: Orthotope String,
                 getFirstBranchLastLeaf :: String
                } deriving (Show, Eq)
