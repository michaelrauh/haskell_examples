module MatrixTools
    ( removeLeftColumn,
    getRightColumn,
    getLeftColumn,
    removeRightColumn,
    getTopRight,
    getBottomLeft,
    getLeftColumnList,
    getRightColumnList
    ) where

import Data.Matrix as M
import qualified Data.Vector as V

type StringMatrix = M.Matrix String

removeLeftColumn :: StringMatrix -> StringMatrix
removeLeftColumn m = M.submatrix 1 (nrows m) 2 (ncols m) m

getRightColumn :: StringMatrix -> StringMatrix
getRightColumn m = M.submatrix 1 (nrows m) (ncols m) (ncols m) m

getLeftColumn :: StringMatrix -> StringMatrix
getLeftColumn m = M.submatrix 1 (nrows m) 1 1 m

removeRightColumn :: StringMatrix -> StringMatrix
removeRightColumn m = M.submatrix 1 (nrows m) 1 (ncols m - 1)  m

getTopRight :: StringMatrix -> String
getTopRight m  = M.getElem 1 (ncols m) m

getBottomLeft :: StringMatrix -> String
getBottomLeft m  = M.getElem (nrows m) 1 m

getLeftColumnList :: StringMatrix -> [String]
getLeftColumnList m = V.toList $ M.getCol 1 m

getRightColumnList :: StringMatrix -> [String]
getRightColumnList m = V.toList $ M.getCol (ncols m) m
