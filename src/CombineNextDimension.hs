module CombineNextDimension (combineNextDimension, Box (Square, Hyper)) where
import qualified Data.Matrix as M
import qualified Data.Map.Strict as Map
import qualified Data.Set as S

type Matrix = M.Matrix String
data Box = Square Matrix | Hyper Box Box deriving (Show, Eq)
type PhraseMap = Map.Map [String] (S.Set String)

combineNextDimension :: PhraseMap -> [Box] -> [Box]
combineNextDimension phraseMap boxList = []

-- combineNextDimension :: Box -> Box -> Box
-- combineNextDimension (Square x) (Square y) = Hyper (Square $ M.fromList 2 2 ["a", "b", "c", "d"]) (Square $ M.fromList 2 2 ["a", "b", "c", "d"])
-- combineNextDimension (Hyper x y) (Hyper a b) = Hyper (Hyper x y) (Hyper a b)
