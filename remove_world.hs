module RemoveWorld where

myWord :: String -> [String]
myWord [] = []
myWord (' ' : xs) = myWord xs
myWord xs = takeWhile (/= ' ') xs : myWord (dropWhile (/= ' ') xs)

myFilter :: String -> [String]
myFilter = filter (\x -> not (elem x ["the", "a", "an"])) . myWord

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList xs = go xs []
  where go (x : xs) result
          | xs == [] = (x : result)
          | otherwise = go xs (x : result)


testFunc :: [Integer] -> Integer
testFunc xs = go xs
  where go (x : xs)
          | xs == [] = x
          | otherwise = testFunc xs

-- line 20 that two xs is the same
-- line 21 the pattern matching could be xs or something else

testFunc' :: [Integer] -> Integer
testFunc' (x : xs) = x
testFunc' _ = (-4)

-- in testFunc' if we pass a empty list [], it will match to second case

reverseList' :: [a] -> [a]
reverseList' [] = []
reverseList' xs = go xs []
  where 
    go :: [a] -> [a] -> [a]
    go [] result = result
    go (x : xs) result = go xs (x : result)

myZip' :: [a] -> [b] -> [(a, b)]
myZip' (x : xs) (y : ys) = (x, y) : myZip' xs ys
myZip' _ _ = []

myZip :: (Eq t1, Eq t2) => [t1] -> [t2] -> [(t1, t2)]
myZip a b = go a b []
  where go (x : xs) (y : ys) result
          | xs == [] = (x, y) : result
          | ys == [] = (x, y) : result
          | otherwise = go xs ys ((x, y) : result)

myZipV2 :: [a] -> [b] -> [(a, b)]
myZipV2 a b = reverseList' (go a b [])
  where
    go :: [a] -> [b] -> [(a, b)] -> [(a, b)]
    go [] _ result = result
    go _ [] result = result
    go (x : xs) (y : ys) result = go xs ys ((x, y) : result)

myZipV3 :: [a] -> [b] -> [(a, b)]
myZipV3 _ [] = []
myZipV3 [] _ = []
myZipV3 (x : xs) (y : ys) = (x, y) : myZipV3 xs ys


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys
myZipWith f _ _ = []




