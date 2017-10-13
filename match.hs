f :: Int -> Int
f = \x -> x + 1

addFive :: Int -> Int -> Int
addFive = \x -> \y -> (if x > y then y else x) + 5