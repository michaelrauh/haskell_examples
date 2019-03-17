module Orthotope (Box (Square, Hyper)) where

import qualified Data.Matrix as M
type Matrix = M.Matrix String

data Box = Square Matrix | Hyper Box Box deriving (Show, Eq)
