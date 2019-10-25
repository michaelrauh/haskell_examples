module Main where
import Search

main :: IO ()
main = do
  contents <- getContents
  putStr $ show $ buildFirstBoxes contents
