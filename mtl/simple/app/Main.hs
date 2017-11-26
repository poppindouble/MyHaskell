module Main where

import Lib
import Control.Monad.State

type ArrayState = [Int]
type ArrayValue = ()

insertArray :: Int -> State ArrayState ArrayValue
insertArray x = do
  currentState <- get
  put (x : currentState)

doIt3TimesInsert :: State ArrayState ArrayValue
doIt3TimesInsert = do
  insertArray 3
  insertArray 4
  insertArray 5


initState = []

main :: IO ()
main = do
  print $ execState doIt3TimesInsert initState