import Data.Char

vigenere :: String -> String -> String
vigenere xs ys = helper xs (cycle ys)

helper :: String -> String -> String
helper [] _ = []
helper (' ' : xs) ys = ' ' : helper xs ys
helper (x : xs) (y : ys) = (convert x y) : helper xs ys

convert :: Char -> Char -> Char
convert x y = chr $ ((ord x - ord 'A') + (ord y - ord 'A')) `mod` 26 + ord 'A'

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] ys = True
isSubsequenceOf _ [] = False
isSubsequenceOf ax@(x : xs) (y : ys)
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf ax ys


capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map (\x -> (x, capitalString x)) $ words xs

capitalString :: String -> String
capitalString [] = []
capitalString (x : xs) = toUpper x : capitalString xs