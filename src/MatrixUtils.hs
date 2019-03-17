module MatrixUtils
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
    getBottomRow,
    isTranspose
    ) where

import Data.Matrix as M
import qualified Data.Vector as V
import Orthotope

type StringMatrix = M.Matrix String

removeLeftColumn :: StringMatrix -> StringMatrix
removeLeftColumn m = M.submatrix 1 (nrows m) 2 (ncols m) m

getLeftColumn :: StringMatrix -> StringMatrix
getLeftColumn m = M.submatrix 1 (nrows m) 1 1 m

removeRightColumn :: StringMatrix -> StringMatrix
removeRightColumn m = M.submatrix 1 (nrows m) 1 (ncols m - 1) m

getRightColumn :: StringMatrix -> StringMatrix
getRightColumn m = M.submatrix 1 (nrows m) (ncols m) (ncols m) m

removeTopRow :: StringMatrix -> StringMatrix
removeTopRow m = M.submatrix 2 (nrows m) 1 (ncols m) m

removeBottomRow :: StringMatrix -> StringMatrix
removeBottomRow m = submatrix 1 (nrows m -1) 1 (ncols m) m

getBottomRow :: StringMatrix -> StringMatrix
getBottomRow m = submatrix (nrows m) (nrows m) 1 (ncols m) m

getRows :: Box -> [[String]]
getRows m = [[]] -- [V.toList (getRow x m) | x <- [1.. (nrows m)]]

getColumns :: Box -> [[String]]
getColumns m = [[]] -- [V.toList (getCol x m) | x <- [1.. (ncols m)]]

getLeftColumnList :: StringMatrix -> [String]
getLeftColumnList m = V.toList $ M.getCol 1 m

getRightColumnList :: Box -> [String]
getRightColumnList m = [] -- V.toList $ M.getCol (ncols m) m

getBottomRowList :: Box -> [String]
getBottomRowList m = [] -- V.toList $ M.getRow (nrows m) m

getTopRight :: StringMatrix -> String
getTopRight m = m ! (1, ncols m)

getBottomLeft :: StringMatrix -> String
getBottomLeft m = m ! (nrows m, 1)

isTranspose :: StringMatrix -> StringMatrix -> Bool
isTranspose m1 m2 = M.transpose m1 == m2
