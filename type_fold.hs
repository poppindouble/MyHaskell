lefts :: [Either a b] -> [a]
lefts xs = foldr f [] xs
  where
    f :: Either a b -> [a] -> [a]
    f (Left a) xs = a : xs
    f (Right _) xs = xs

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs = foldr f ([], []) xs
  where
    f :: Either a b -> ([a], [b]) -> ([a], [b])
    f (Left a) (x, y) = (a : x, y)
    f (Right b) (x, y) = (x, b : y)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Left a) = Nothing
eitherMaybe'' f eb = Just (either' undefined f eb)

