import Data.List

example = "a b c d a b a b a c a a b c d a b"
type Answer = (String, String, String, String, String)

adjacentWords :: Int -> String -> String -> [String]
adjacentWords offset corpus word =
  let wordList = words corpus
      indices = elemIndices word wordList
      offsetIndices = map(\x -> x + offset) indices
      remaining = filter(\offsetIndex -> offsetIndex > 0 && offsetIndex < (length wordList)) offsetIndices
  in map(\x -> wordList !! x) remaining

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

filterFoldedWords :: [Answer] -> [Answer]
filterFoldedWords answers =
  filter(\(a, b, d, c, a') -> b /= c && a == a') answers

exampleWordList = (words example)

execute = filterFoldedWords $ concat $ map (\w -> foldWord w) $ exampleWordList
