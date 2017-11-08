data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegtive :: Int -> Maybe Int
noNegtive n
          | n >= 0 = Just n
          | otherwise = Nothing


cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString n a w= Cow <$> noEmpty n
                         <*> noNegtive a
                         <*> noNegtive w