module Main where
import Search

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
      firstBoxes = buildFirstBoxes wordList
      b3 = getAllCurrentDimension wordList 2 firstBoxes
      b3x2 = getAllNextDimension wordList b3
      b3x3 = getAllCurrentDimension wordList 2 b3x2
  print $ head b3x3