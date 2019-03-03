module MapBuilder
    ( buildNextWordMap
    ) where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Set as S
import qualified Data.Map.Strict as Map

buildNextWordMap :: [String] -> Map.Map String (S.Set String)
buildNextWordMap stringList = Map.fromList [("a", S.singleton "b")]
