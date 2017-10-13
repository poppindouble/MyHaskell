module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"

digits :: Int -> [Int]
digits num = go num []
  where go num result
          | num == 0 = result
          | otherwise = go (div num 10) ([mod num 10] ++ result)

wordNumber :: Int -> String
wordNumber = 
  concat . intersperse "-" . map digitToWord . digits