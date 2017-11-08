newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity a) = Identity (f a)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if (length s) > maxLen
                            then Nothing
                            else Just s

newtype Name = Name String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s