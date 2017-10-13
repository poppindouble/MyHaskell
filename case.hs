avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.6 = 'B'
  | otherwise = 'C'
  where y = x / 100

numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1


tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where xLast = fst $ divMod x 10
        d     = snd $ divMod xLast 10

hundredsDigit :: Integral a => a -> a
hundredsDigit x = d
  where xLast = fst $ divMod x 100
        d     = snd $ divMod xLast 10

-- myTensDigit :: Integral a => a -> a
-- myTensDigit x = d
--   where d = snd $ divMod (fst $ divMod x 10) 10

foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y z =
  case z of
    True -> x
    False -> y

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y z
  | z = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)