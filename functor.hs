-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- fmap :: Functor f => (m -> n) -> (f m -> f n)
-- fmap :: Functor g => (f m -> f n) -> (g (f m) -> g (f n))

-- fmap.fmap :: (m -> n) -> (g (f m) -> g (f n)) 

-- the function above means, if i give a function (m -> n) to fmap.fmap, then I can transform a structure like (g (f m)), structure like [Maybe String]

import Test.QuickCheck
import Test.Hspec
import Test.QuickCheck.Function

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "Woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap .fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

-- =======================================================================
-- test case for Identity
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity x = fmap id x == x

type FunctorIdentity = [Int] -> Bool

-- test case for Compose
functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

type FunctorCompose = [Int] -> Bool

-- another way to test it
functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- =======================================================================

newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- 

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

-- 

data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 

data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- 

data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
-- 


newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v


-- data Wrap f a = Wrap (f a) deriving (Eq, Show)

-- data Tuple a b = Tuple a b deriving (Eq, Show)
-- newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- instance Functor (Flip Tuple a) where
--   fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b


main :: IO ()
main = do
  print (replaceWithP' lms)
  print (liftedReplace lms)
  print (liftedReplace' lms)
  print (twiceLifted lms)
  print (twiceLifted' lms)
  print (thriceLifted lms)
  print (thriceLifted' lms)
  quickCheck(functorIdentity :: FunctorIdentity)
  let c = functorCompose (+1) (*2)
  quickCheck(c :: FunctorCompose)

  quickCheck(functorCompose' :: IntFC)