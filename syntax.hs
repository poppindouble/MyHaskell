module Syntax where

myFunc :: String -> Int
myFunc x = plus xLength 1
  where
    plus = (+)
    xLength = length x

myFunc2 xs = w `x` 1
  where w = length xs
        x = (+)

idFunc = \x -> x
myHead = \(head , tail) -> head