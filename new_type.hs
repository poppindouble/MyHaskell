class TooMany a where
  tooMany :: a -> Bool

newtype Goats = Goats (Int, Int) deriving Show

instance TooMany Goats where
  tooMany (Goats (a, b)) = (a + b) > 40