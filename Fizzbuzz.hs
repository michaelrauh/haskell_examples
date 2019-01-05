fizzbuzz :: (Integral a, Show a) => a -> [Char]
fizzbuzz x
    | x `mod` 15 == 0 = "Fizzbuzz"
    | x `mod` 5 == 0  = "Buzz"
    | x `mod` 3 == 0  = "Fizz"
    | otherwise       = show x

main = print [fizzbuzz x | x <- [1..100]]
