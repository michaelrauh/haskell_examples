module BoxData (Box (Box, getColumn, getOrthotope, getLines, getCenter1, getCenter2, getDiagonals)) where
import Orthotope
import qualified Data.Set as S

data Box = Box { getOrthotope :: Orthotope String,
                 getLines :: Orthotope String,
                 getColumn :: Orthotope String,
                 getCenter1 :: Orthotope String,
                 getCenter2 :: Orthotope String,
                 getDiagonals :: [S.Set String]
                } deriving (Eq, Ord)

instance Show Box where
  show (Box o l c cen1 cen2 diag) = show o