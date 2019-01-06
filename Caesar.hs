import Data.Char

encrypt :: String -> Int -> String
encrypt plaintext shift =
  map (chr . (+ shift) . ord) plaintext

dycrypt :: String -> Int -> String
dycrypt plaintext shift =
  map (chr . (subtract shift) . ord) plaintext

main = print $ dycrypt (encrypt "This is an example" 3) 3
