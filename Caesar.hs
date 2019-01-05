import Data.Char

encrypt :: String -> Int -> String
encrypt plaintext shift =
  map (chr . (+ shift) . ord) plaintext

main = print $ encrypt "This is an example" 3
