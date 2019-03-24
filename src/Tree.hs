module Tree where
import Data.Sequence

data Tree = Leaf String | Twig (Seq Tree) | Branch (Seq Tree) deriving (Show)
