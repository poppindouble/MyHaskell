module Mychar where

import Data.Char

filterLowerCase :: String -> String
filterLowerCase x = filter isUpper x

myFilter :: String -> [String]
myFilter x = filter (\c -> not (elem c ["the", "an", "a"])) $ words x

capFirstOne :: String -> String
capFirstOne [] = []
capFirstOne (x : xs) = toUpper x : xs

capAllChar :: String -> String
capAllChar [] = []
capAllChar (x : xs) = toUpper x : capAllChar xs

capFirstOneWithHead :: String -> Maybe Char
capFirstOneWithHead [] = Nothing
capFirstOneWithHead xs = Just (toUpper . head $ xs)

