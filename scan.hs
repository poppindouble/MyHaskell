fibs = 0 : scanl (+) 1 fibs
fib = scanl (+) 1 fib
fibsN x = fibs !! x

myFibs = takeWhile (\x -> x < 100) fibs

myScanLeft :: (a -> b -> a) -> a -> [b] -> [a]
myScanLeft f acc [] = acc : []
myScanLeft f acc (x : xs) = acc : myScanLeft f (f acc x) xs

myScanRight :: (a -> b -> b) -> b -> [a] -> [b]
myScanRight f initValue [] = initValue : []
myScanRight f initValue (x : xs) = f x (head result) : result
 where result = myScanRight f initValue xs

myFoldLeft :: (a -> b -> a) -> a -> [b] -> a
myFoldLeft f acc [] = acc
myFoldLeft f acc (x : xs) = myFoldLeft f (f acc x) xs

myFoldRight :: (a -> b -> b) -> b -> [a] -> b
myFoldRight f initValue [] = initValue
myFoldRight f initValue (x : xs) = f x (myFoldRight f initValue xs)

myFactorial = scanl (*) 1 [1..]