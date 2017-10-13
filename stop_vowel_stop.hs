myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] =[]
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myListComprehension :: [a] -> [b] -> [(a, b)]
myListComprehension [] _  = []
myListComprehension (x : xs) list = singleElemComprehension x list ++ myListComprehension xs list

singleElemComprehension :: a -> [b] -> [(a, b)]
singleElemComprehension n list = foldr (\x -> \y -> (n, x) : y) [] list

stops = "pbtdkg"
vowels = "awiou"

stopVowelStopComprehension :: [a] -> [b] -> [a] -> [(a, b, a)]
stopVowelStopComprehension l1 l2 l3 = [(x, y, z) | x <- l1, y <- l2, z <- l3]

stopVowelStopComprehensionWithP :: [Char] -> [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStopComprehensionWithP l1 l2 l3 = [(x, y, z) | x <- l1, y <- l2, z <- l3, x == 'p']

myOrFoldRight :: [Bool] -> Bool
myOrFoldRight xs = foldr (||) False xs

myOrFoldLeft :: [Bool] -> Bool
myOrFoldLeft xs = foldl (||) False xs

myAnyFoldRight :: (a -> Bool) -> [a] -> Bool
myAnyFoldRight f xs = foldr (\x -> \y -> f x || y) False xs

myAnyFoldLeft :: (a -> Bool) -> [a] -> Bool
myAnyFoldLeft f xs = foldl (\x -> \y -> f y || x) False xs

myElemFoldLeft :: Eq a => a -> [a] -> Bool
myElemFoldLeft n xs = foldl (\x -> \y -> x || y == n) False xs

myElemFoldRight :: Eq a => a -> [a] -> Bool
myElemFoldRight n xs = foldr (\x -> \y -> x == n || y) False xs

myReverseFoldRight :: [a] -> [a]
myReverseFoldRight xs = foldr (\x -> \y -> y ++ [x]) [] xs

myReverseFoldLeft :: [a] -> [a]
myReverseFoldLeft xs = foldl (flip (:)) [] xs

myMapFoldRight :: (a -> b) -> [a] -> [b]
myMapFoldRight f xs = foldr (\x -> \y -> (f x) : y) [] xs

myMapFoldLeft :: (a -> b) -> [a] -> [b]
myMapFoldLeft f xs = foldl (\x -> \y -> x ++ [(f y)]) [] xs

myFilterRight :: (a -> Bool) -> [a] -> [a]
myFilterRight f xs = foldr (\x -> \y -> if f x then x : y else y) [] xs

myFilterLeft :: (a -> Bool) -> [a] -> [a]
myFilterLeft f xs = foldl (\x -> \y -> if f y then x ++ [y] else x) [] xs

mySquishRight :: [[a]] -> [a]
mySquishRight xs = foldr (++) [] xs

mySquishLeft :: [[a]] -> [a]
mySquishLeft xs = foldl (++) [] xs

mySquishMapFoldRight :: (a -> [b]) -> [a] -> [b]
mySquishMapFoldRight f xs = foldr (\x -> \y -> f x ++ y) [] xs

mySquishMapFoldLeft :: (a -> [b]) -> [a] -> [b]
mySquishMapFoldLeft f xs = foldl (\x -> \y -> x ++ f y) [] xs

squishAgain :: [[a]] -> [a]
squishAgain xs = mySquishMapFoldRight id xs

myMaximumByFoldRight :: (a -> a -> Ordering) -> [a] -> a
myMaximumByFoldRight _ [] = undefined
myMaximumByFoldRight _ [x] = x
myMaximumByFoldRight f (x : xs) = foldr fo x xs
  where fo a b
          | f a b == GT = a
          | otherwise = b

myMaximumByFoldLeft :: (a -> a -> Ordering) -> [a] -> a
myMaximumByFoldLeft _ [] = undefined
myMaximumByFoldLeft _ [x] = x
myMaximumByFoldLeft f (x : xs) = foldl fo x xs
  where fo a b
          | f a b == GT = a
          | otherwise = b

