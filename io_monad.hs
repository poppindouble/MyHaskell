import Control.Applicative
import System.Random

main = do
  x <- getLine
  y <- getLine
  putStrLn $ myConcat x y

myConcat :: String -> String -> String
myConcat x y = x ++ " " ++ y

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = go [] n 0
  where
    go :: [Int] -> Int -> Int -> IO [Int]
    go result n counter
      | counter == n = return result
      | otherwise = go (counter : result) n (counter + 1)

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice g = ((m, n), newGenerator)
  where
    (m, g') = random g
    (n, newGenerator) = random g'