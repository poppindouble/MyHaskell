module Main where

import Test.Hspec
import Test.QuickCheck

multiply :: (Eq a, Num a) => a -> a -> a
multiply m n = go m n 0
  where
    go :: (Eq a, Num a) => a -> a -> a -> a
    go m 0 result = result
    go m n result = go m (n - 1) (result + m)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [True, False]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

-- this will fail compiling
-- otherTest :: (Num a) => IO a
-- otherTest = return (3 :: Int)

-- this will work because :t 3 is Num t => t
otherTest :: (Num a) => IO a
otherTest = return 3

otherTest' :: (Fractional a) => IO a
otherTest' = return 3.0

genInt :: Gen Int
genInt = do
  a <- arbitrary :: Gen Int
  return a

test :: (Arbitrary a) => a -> Gen a
test x = return x

test' :: (Arbitrary a, Arbitrary b) => a -> b -> Gen (a, b)
test' x y = return (x, y)

main :: IO ()
main = hspec $ do
  describe "multiply" $ do
    it "3 * 4 is equal to 12" $ do
      multiply 3 4 `shouldBe` 12