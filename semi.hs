import Test.QuickCheck
import Data.Semigroup
import Test.Hspec

-- 
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

assocTestFunc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
assocTestFunc a b c = (a <> b) <> c == a <> (b <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssocString = Identity String -> Identity String -> Identity String -> Bool

-- 

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup(Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssocString = Three String String String -> Three String String String -> Three String String String -> Bool

-- 

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssorcString = BoolConj -> BoolConj -> BoolConj -> Bool


-- 
data Or a b = 
    Fst a
  | Snd b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup(Or a b) where
  (Snd a) <> _ = Snd a
  _ <> (Snd a) = Snd a
  _ <> b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary(Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssorcString = Or String String -> Or String String -> Or String String -> Bool

-- 

newtype Combine a b = Combine {unCombine :: (a -> b)}

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x <> g x)

-- 

newtype Comp a = Comp {unComp :: (a -> a)}

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (\x -> f x <> g x)

-- 

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Failure' a) <> (Failure' b)  = Failure' (a <> b)
  (Failure' a) <> _            = Failure' a
  _           <> (Failure' a)  = Failure' a
  a           <> _            = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Failure' a), (Success' b)]

type ValidationAssoc = Validation String Ordering -> Validation String Ordering -> Validation String Ordering -> Bool

main :: IO ()
main = do
  quickCheck (assocTestFunc :: TrivialAssoc)
  quickCheck (assocTestFunc :: IdentityAssocString)
  quickCheck (assocTestFunc :: ThreeAssocString)
  quickCheck (assocTestFunc :: BoolConjAssorcString)
  quickCheck (assocTestFunc :: OrAssorcString)
  quickCheck (assocTestFunc :: ValidationAssoc)