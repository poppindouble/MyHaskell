cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

dividedBy :: Integer -> Integer -> (Integer, Integer)
dividedBy num denom = go num denom 0
  where go n d t
          | n >= d = go (n - d) d (t + 1)
          | otherwise = (t, n)

sumAll :: (Eq a, Num a) => a -> a
sumAll num = go num 0
  where go x count
          | x /= 0 = go (x - 1) (count + x)
          | otherwise = count

multi :: Integer -> Integer -> Integer
multi x y = go x y 0
  where go x y result
          | y > 0 = go x (y - 1) (result + x)
          | otherwise = result

data DividedResult =   Result Integer
                     | DividedByZero

myDividedBy :: Integer -> Integer -> DividedResult
myDividedBy num denom
  | denom == 0 = DividedByZero
  | otherwise = go (abs num) (abs denom) 0 ((signum num) * (signum denom))
    where go n d count s
            | n < d = Result (s * count)
            | otherwise = go (n - d) d (count + 1) s

mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ n + 11