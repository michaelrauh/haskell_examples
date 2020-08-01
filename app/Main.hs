module Main where
import Search

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
      firstBoxes = buildFirstBoxes wordList
      b2x2 = getAllNextDimension wordList firstBoxes
      answer = getAllCurrentDimension wordList 2 b2x2
  print answer