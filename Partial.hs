fizzbuzz :: (Integral a, Show a) => a -> [Char]
fizzbuzz x = both x ?? fizz x ?? buzz x ?? neither x

main = print [fizzbuzz x | x <- [1..100]]

fizz x
    | x `mod` 3 == 0 = Just "Fizz"
    | otherwise      = Nothing

buzz x
    | x `mod` 5 == 0 = Just "Buzz"
    | otherwise      = Nothing

both x
    | x `mod` 15 == 0 = Just "FizzBuzz"
    | otherwise       = Nothing

neither x = show x

infixr 3 ??
(??) :: Maybe a -> a -> a
Just x ?? _ = x
Nothing ?? y = y
