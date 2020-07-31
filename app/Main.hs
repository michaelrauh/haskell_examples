module Main where
import Search

main :: IO ()
main = do
  contents <- getContents
  let wordList = words contents
      firstBoxes = buildFirstBoxes wordList
      b2x2 = getAllNextDimension wordList firstBoxes
      b2x2x2 = getAllNextDimension wordList b2x2
  putStr $ show b2x2x2


