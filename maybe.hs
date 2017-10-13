isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

foldCatMaybes :: [Maybe a] -> [a]
foldCatMaybes xs = foldr f [] xs
  where
    f :: Maybe a -> [a] -> [a]
    f Nothing xs = xs
    f (Just a) xs = a : xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = foldr f (Just []) xs
  where
    f :: Maybe a -> Maybe [a] -> Maybe [a]
    f Nothing _ = Nothing
    f _ Nothing = Nothing
    f (Just a) (Just b) = Just (a : b)

