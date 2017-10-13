module Greeting where

greetIfCool :: String -> IO()
greetIfCool coolness =
  if cool
    then putStrLn "Hell yeah"
  else
    putStrLn "Hell no"
  where
    cool = coolness == "downright frosty yo"

myFirst :: (a, b) -> a
myFirst (a, b) = a

tupFunc :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
tupFunc (a, b) (c, d) = ((a + c), (b ++ d))

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x =
  if x > 0
    then x
  else
    negate x

myFirAndSec :: (a, b) -> (c, d) -> ((b, d), (a, c))
myFirAndSec x y =
  ((snd x, snd y), (fst x, fst y))