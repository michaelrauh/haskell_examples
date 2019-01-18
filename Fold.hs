import Data.List

example = "this is a string with repeated words. The string is long"
type Answer = (String, String, String, String, String)

adjacentWords :: Int -> String -> String -> [String]
adjacentWords offset corpus word =
  let wordList = words corpus
      indices = elemIndices word wordList
  in map(\x -> wordList !! (x + offset)) indices

nextWords :: String -> [String]
nextWords = adjacentWords 1 example

prevWords :: String -> [String]
prevWords = adjacentWords (-1) example

foldWord :: String -> [Answer]
foldWord a = do
      b <- nextWords a
      d <- nextWords b
      c <- prevWords d
      a' <- prevWords c
      return (a, b, d, c, a')
