myIterate :: (a -> a) -> a -> [a]
myIterate f a = go f [a]
  where
     go :: (a -> a) -> [a] -> [a]
     go f (x : xs) = [x] ++ go f ((f x) : x : xs)

myIterate' :: (a -> a) -> a -> [a]
myIterate' f a = a : myIterate' f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                  Nothing -> []
                  Just (a, b) -> a : myUnfoldr f b

myBetterIterate :: (a -> a) -> a -> [a]
myBetterIterate f a = myUnfoldr (\a -> Just (a, f a)) a