myEnumFromTo :: Integer -> Integer -> [Integer]
myEnumFromTo start stop = go start stop
  where go x y
          | y < x = []
          | otherwise = x : go (succ x) y

myWords :: String -> [String]
myWords [] = []
myWords (' ' : xs) = myWords xs
myWords xs = takeWhile (/= ' ') xs : myWords (dropWhile (/= ' ') xs)

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines ('\n' : xs) = myLines xs
myLines xs = takeWhile (/= '\n') xs : myLines (dropWhile (/= '\n') xs)

myParser :: String -> Char -> [String]
myParser [] _ = []
myParser a@(x : xs) c
  | x == c = myParser xs c
  | otherwise = takeWhile (/= c) a : myParser (dropWhile (/= c) a) c

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
myResult = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

nthElement :: [a] -> Int -> Maybe a
nthElement [] _ = Nothing
nthElement (x : xs) i
  | i <= 0 = Nothing
  | i == 1 = Just x
  | otherwise = nthElement xs (i - 1)

filterResult = filter (\x -> (rem x 3) == 0) [1..30]

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList xs = go xs []
  where go (y : ys) result
          | ys == [] = (y : result)
          | otherwise = go ys (y : result)

filterMultiple3 :: [Integer] -> [Integer]
filterMultiple3 = filter (\x -> (rem x 3) == 0)

lengthFilterMultiple3 :: [Integer] -> Int
lengthFilterMultiple3 = length . filter (\x -> (rem x 3) == 0)


