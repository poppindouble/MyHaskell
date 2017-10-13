myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs)
  | x == True = myAnd xs
  | otherwise = False

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs)
  | x == False = myOr xs
  | otherwise = True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x : xs)
  | f x == True = True
  | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x : xs)
  | a == x = True
  | otherwise = myElem a xs

myReverse :: [a] -> [a]
myReverse xs = f xs []
  where 
    f :: [a] -> [a] -> [a]
    f [] result = result
    f (x : xs) result = f xs (x : result)

mySquish :: [[a]] -> [a]
mySquish [] = []
mySquish (x : xs) = x ++ mySquish xs

mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap f [] = []
mySquishMap f (x : xs) = f x ++ mySquishMap f xs

mySquishAgain :: [[a]] -> [a]
mySquishAgain xs = mySquishMap id xs

-- in this style, you will get your final result as a solid value, it can not be reduced any more, so the (go f b [] = b) will directly return
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ (x : []) = x
myMaximumBy f (x : xs) = go f x xs
  where go f b (x : xs)
          | f b x == GT = go f b xs
          | otherwise = go f x xs
        go f b [] = b

-- in this style, the result will be like compare 8 (compare 4 (compare 10 (compare 13 (4)))), it still can be evaluated
myMaximumByV2 :: (a -> a -> Ordering) -> [a] -> a
myMaximumByV2 _ [] = undefined
myMaximumByV2 _ (x : []) = x
myMaximumByV2 f (x : y : []) = if f x y == GT
                                  then x
                                  else y
myMaximumByV2 f (x : xs) = if (f x (myMaximumByV2 f xs)) == GT
                               then x
                               else myMaximumByV2 f xs

myMaximumByV3 :: (a -> a -> Ordering) -> [a] -> a
myMaximumByV3 _ [] = undefined
myMaximumByV3 _ (x : []) = x
myMaximumByV3 f (x : xs)
  | f x (myMaximumByV3 f xs) == GT = x
  | otherwise = myMaximumByV3 f xs


myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy f (x : []) = x
myMinimumBy f (x : xs) = go f x xs
  where go f result (x : xs)
          | f result x == LT = go f result xs
          | otherwise = go f x xs
        go f result [] = result

myMinimumByV2 :: (a -> a -> Ordering) -> [a] -> a
myMinimumByV2 _ [] = undefined
myMinimumByV2 f (x : []) = x
myMinimumByV2 f (x : xs)
  | f x (myMinimumByV2 f xs) == LT = x
  | otherwise = myMinimumByV2 f xs

myFoldrInterpreter :: (Show a) => [a] -> String
myFoldrInterpreter xs = foldr (\x -> \y -> concat ["(", x, "+", y, ")"]) "0"  $ (map show xs)

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f xs = foldr (\x b -> f x || b) False xs

viewMyAny :: (a -> Bool) -> [a] -> [Bool]
viewMyAny f xs = scanr (\x -> \y ->  f x || y) False xs

