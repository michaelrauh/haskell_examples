import Data.List

adjacentWords :: Int -> String -> String -> [String]
adjacentWords offset corpus word =
  let wordList = words corpus
      indices = elemIndices word wordList
  in map(\x -> wordList !! (x + offset)) indices

nextWords :: String -> String -> [String]
nextWords = adjacentWords 1

prevWords :: String -> String -> [String]
prevWords = adjacentWords (-1)
