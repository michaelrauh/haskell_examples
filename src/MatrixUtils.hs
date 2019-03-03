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
    getBottomRow
    ) where

import Data.Matrix as M
import qualified Data.Vector as V

type StringMatrix = M.Matrix String

removeLeftColumn :: StringMatrix -> StringMatrix
removeLeftColumn m = M.submatrix 1 (nrows m) 2 (ncols m) m

getLeftColumn :: StringMatrix -> StringMatrix
getLeftColumn m = M.submatrix 1 (nrows m) 1 1 m

removeRightColumn :: StringMatrix -> StringMatrix
removeRightColumn m = M.submatrix 1 (nrows m) 1 (ncols m - 1) m

getRightColumn :: StringMatrix -> StringMatrix
getRightColumn m = M.submatrix 1 (nrows m) (ncols m) (ncols m) m

getTopRight :: StringMatrix -> String
getTopRight m = m ! (1, ncols m)

getBottomLeft :: StringMatrix -> String
getBottomLeft m = m ! (nrows m, 1)

getLeftColumnList :: StringMatrix -> [String]
getLeftColumnList m = ["foo"]

getRightColumnList :: StringMatrix -> [String]
getRightColumnList m = ["foo"]

getRows :: StringMatrix -> [[String]]
getRows m = [["foo"]]

removeTopRow :: StringMatrix -> StringMatrix
removeTopRow m = M.fromList 1 1 ["1"]

removeBottomRow :: StringMatrix -> StringMatrix
removeBottomRow m = M.fromList 1 1 ["1"]

getColumns :: StringMatrix -> [[String]]
getColumns m = [["foo"]]

getBottomRowList :: StringMatrix -> [String]
getBottomRowList m = ["foo"]

getBottomRow :: StringMatrix -> StringMatrix
getBottomRow m = M.fromList 1 1 ["1"]
