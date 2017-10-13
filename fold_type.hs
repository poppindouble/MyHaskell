import Data.Time

data DatabaseItem =   DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)),
    DbNumber 9001,
    DbString "Hello, world!",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr f [] xs
  where
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f (DbDate a) b = a : b
    f _ b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr f [] xs
  where
    f :: DatabaseItem -> [Integer] -> [Integer]
    f (DbNumber a) b = a : b
    f _ b = b

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr f 0 xs
  where
    f :: DatabaseItem -> Integer -> Integer
    f (DbNumber a) b = a + b
    f _ b = b

avgDb :: [DatabaseItem] -> Double
avgDb xs = average (filterDbNumber xs)
  where 
    average :: [Integer] -> Double
    average [] = 0
    average xs = fromIntegral(sum xs) / fromIntegral (length xs)