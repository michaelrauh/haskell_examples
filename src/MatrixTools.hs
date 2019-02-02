module MatrixTools
    ( removeLeftColumn,
    getRightColumn,
    getLeftColumn,
    removeRightColumn,
    getTopRight,
    getBottomLeft,
    getLeftColumnList,
    getRightColumnList,
    getRows,
    removeTopRow,
    removeBottomRow,
    getColumns,
    getBottomRowList,
    getBottomRow
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

getRows :: StringMatrix -> [[String]]
getRows m = [V.toList (getRow x m) | x <- [1.. (nrows m)]]

removeTopRow :: StringMatrix -> StringMatrix
removeTopRow m = M.submatrix 2 (nrows m) 1 (ncols m) m

removeBottomRow :: StringMatrix -> StringMatrix
removeBottomRow m = submatrix 1 (nrows m -1) 1 (ncols m) m

getColumns :: StringMatrix -> [[String]]
getColumns m = [V.toList (getCol x m) | x <- [1.. (ncols m)]]

getBottomRowList :: StringMatrix -> [String]
getBottomRowList m = V.toList $ M.getRow (nrows m) m

getBottomRow :: StringMatrix -> StringMatrix
getBottomRow m = submatrix (nrows m) (nrows m) 1 (ncols m) m
