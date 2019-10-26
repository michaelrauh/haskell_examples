module Main where
import Search

main :: IO ()
main = do
  contents <- getContents
  let firstBoxes = buildFirstBoxes $ words contents
  putStr $ show firstBoxes


