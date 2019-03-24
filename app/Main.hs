module Main where

import Data.List
import Control.Monad
import qualified Data.Matrix as M
import MapBuilder

main :: IO ()
main = do
  contents <- getContents
  putStr contents
