module Cipher where

import Data.Char

myRightCharShift :: Char -> Int -> Char
myRightCharShift c i
  | not (isAlpha c) = c
  | ord c + i > ord 'z' = chr (ord 'a' + offSet)
  | otherwise = chr $ ord c + i
    where 
      offSet = mod (ord c + i - ord 'z' - 1) 26

caesar :: String -> Int -> String
caesar [] _ = []
caesar (x : xs) i = myRightCharShift x i : caesar xs i

myLeftCharShift :: Char -> Int -> Char
myLeftCharShift c i
  | not (isAlpha c) = c
  | ord c - i < ord 'a' = chr (ord 'z' - offSet)
  | otherwise = chr $ ord c - i
    where 
      offSet = mod (ord 'a' - ord c + i - 1) 26

uncaesar :: String -> Int -> String
uncaesar [] _ = []
uncaesar (x : xs) i = myLeftCharShift x i : uncaesar xs i

