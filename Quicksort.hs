main = print $ take 1 $ quicksort [10000,9999..0]

quicksort :: (Ord a) => [a] -> [a]
quicksort []  = []
quicksort (pivot:rest) =
  let left  = quicksort $ filter (<= pivot) rest
      right = quicksort $ filter (> pivot) rest
  in left ++ [pivot] ++ right
