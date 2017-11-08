import Data.Monoid (Monoid, Sum)
import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- 

newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup(Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance (Monoid a, Semigroup a) => Monoid(Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary(Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup(Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Semigroup a, Monoid b, Semigroup b) => Monoid(Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary(Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String Ordering -> Two String Ordering -> Two String Ordering -> Bool

-- 
newtype BoolConj =
  BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 

newtype Combine a b = Combine { unCombine :: (a -> b)}

instance Semigroup b => Semigroup(Combine a b) where
  Combine f <> Combine g = Combine (\x -> f x <> g x)

instance (Monoid b, Semigroup b) => Monoid(Combine a b) where
  mempty = Combine (\x -> mempty)
  mappend = (<>)

-- 

newtype Comp a = Comp { unComp :: (a -> a)}

instance Semigroup a => Semigroup(Comp a) where
  Comp f <> Comp g = Comp (\x -> f x <> g x)

instance (Monoid a, Semigroup a) => Monoid(Comp a) where
  mempty = Comp (id mempty)

  mappend = (<>)

-- 

newtype Mem s a = Mem { runMem :: s -> (a, s)}

instance (Semigroup a) => Semigroup (Mem s a)  where
  Mem f <> Mem g = Mem $ \x -> let (a, b) = g x
                                   (c, d) = f x
                                 in (a <> c, d)

instance (Monoid a, Semigroup a) => Monoid(Mem s a) where
  mempty = Mem (\s -> (mempty, s))

  mappend = (<>)

main :: IO()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidRightIdentity :: Two String Ordering -> Bool)
  quickCheck (monoidLeftIdentity :: Two String Ordering -> Bool)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)