import Data.List (intercalate)

notThe :: String -> Maybe String
notThe s
       | s == "the" = Nothing
       | otherwise = Just s

replaceThe :: String -> String
replaceThe str = intercalate " " $ map athe $ map notThe $ words str
  where
    athe :: Maybe String -> String
    athe Nothing = "a"
    athe (Just x) = x

countTheBeforeVowel :: [String] -> Integer
countTheBeforeVowel (y : []) = 0
countTheBeforeVowel (x : y : xs)
  | x /= "cow" = countTheBeforeVowel (y : xs)
  | otherwise = case startWithVowel y of
                True -> 1 + countTheBeforeVowel (y : xs)
                False -> countTheBeforeVowel (y : xs)

startWithVowel :: String -> Bool
startWithVowel str = head str `elem` "aeiou"

countVowels :: String -> Int
countVowels str = length $ filter isVowel str

newtype Word' =
  Word' String
  deriving (Eq, Show)

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

mkWord :: String -> Maybe Word'
mkWord str
       | (length str) - (countVowels str) < countVowels str = Nothing
       | otherwise = Just (Word' str)


data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
             | n < 0 = Nothing
             | otherwise = Just (go n)
  where
    go :: Integer -> Nat
    go n
       | n == 0 = Zero
       | otherwise = Succ $ go (n - 1)
