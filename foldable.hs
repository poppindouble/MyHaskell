import Data.Foldable
import Data.Monoid

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

mySum :: (Foldable t, Num a) => t a -> a
mySum x = getSum $ foldMap Sum x

myProduct :: (Foldable t, Num a) => t a -> a
myProduct x = getProduct $ foldMap Product x

myElem :: (Foldable t, Eq a) => a -> t a -> Bool
myElem x xs = getAny $ foldMap (Any . (== x)) xs

data Min a = Min { getMin :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  Min Nothing `mappend` x = x
  x `mappend` Min Nothing = x
  mappend (Min a) (Min b) = Min (min a b)

myMinimum :: (Foldable t, Ord a) => t a -> Maybe a
myMinimum x = getMin $ foldMap (Min . Just) x


data Max a = Max { getMax :: Maybe a } deriving (Eq, Show)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  mappend (Max Nothing) a = a
  mappend a (Max Nothing) = a
  mappend (Max a) (Max b) = Max (max a b)

myMaximum :: (Foldable t, Ord a) => t a -> Maybe a
myMaximum xs = getMax $ foldMap (Max . Just) xs

myNull :: (Foldable t) => t a -> Bool
myNull = foldr (\x -> \y -> False) True

myLength :: (Foldable t) => t a -> Int
myLength = foldr (\x -> \y -> y + 1) 0

myToList :: (Foldable t) => t a -> [a]
myToList = foldr (\x -> \y -> x : y) []

myFold :: (Foldable t, Monoid m) => t m -> m
myFold = foldMap id

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = foldr (mappend . f) mempty






