import Prelude hiding (Left, Right)
import Data.Monoid

data Nope a =
  NopeDotJpg
  deriving (Show, Eq)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure x = NopeDotJpg
  (<*>) _ NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) NopeDotJpg _ = NopeDotJpg


data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left a) = Left (f a)
  fmap f (Right b) = Right b

instance Applicative (PhhhbbtttEither b) where
  pure = Left
  (<*>) (Right a) _ = Right a
  (<*>) _ (Right a) = Right a
  (<*>) (Left f) (Left a) = Left (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (>>=) (Right a) _ = Right a
  (>>=) (Left a) f = f a


newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

instance Applicative (Identity) where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad (Identity) where
  return = pure
  (>>=) (Identity a) f = f a


data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil
  mappend a Nil = a
  mappend Nil a = a
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys

instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative (List) where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = (fmap f xs) <> (fs <*> xs)

instance Monad (List) where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = (f x) <> (xs >>= f)

