main = print [if x `mod` 15 == 0 then "Fizzbuzz" else if x `mod` 5 == 0 then "Buzz" else if x `mod` 3 == 0 then "Fizz" else show x | x <- [1..100]]
