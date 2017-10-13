module Func where
k (x, y) = x
k1 = k (3, 10)
k2 = k ("three", 4)
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c ,f))

funcZ x =
  case x + 1 == 1 of
    True -> "Awesome"
    False -> "Wut"

pal xs =
  case y of
    True -> "yes"
    False -> "No"
  where y = xs == reverse xs

functionC x y = if (x > y) then x else y

functionC' x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n = if even n then (n + 2) else n

ifEvenAdd2' n =
  case even n of
    True -> n + 2
    False -> n

num x =
  case compare x 0 of
    LT -> -1
    GT -> 1