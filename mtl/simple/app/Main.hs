module Main where

import Lib
import Control.Monad.State

type ArrayState = [Int]
type ArrayValue = ()

data Direction = North | West | South | East
data Explorer = Explorer Int Int Direction

test :: Explorer -> Int
test (Explorer x y d) = x

insertArray :: Int -> State ArrayState ArrayValue
insertArray x = do
  currentState <- get
  put (x : currentState)

doIt3TimesInsert :: State ArrayState ArrayValue
doIt3TimesInsert = do
  insertArray 3
  insertArray 4
  insertArray 5
  insertArrayTest
  insertArrayTest

insertArrayTest :: State ArrayState ArrayValue
insertArrayTest = do
  currentState <- get
  put (0 : currentState)


initState = []

main :: IO ()
main = do
  print $ execState doIt3TimesInsert initState