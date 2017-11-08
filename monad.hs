import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f xs = join $ fmap f xs

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "Name plas: "
  name <- getLine
  putStrLn (name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:" >>
  getLine >>=
  \name ->
  putStrLn(name)

test :: Maybe String
test = do
  y <- Nothing
  return y

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]


twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []
-- -----------------------------------------

data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
           | n >= 0 = Just n
           | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
    then Nothing
    else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weighty' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weighty'
  weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weighty' =
  noEmpty name' >>=
    \nammy -> 
      noNegative age' >>=
        \agey ->
          noNegative weighty' >>=
            \weighty ->
            weightCheck (Cow nammy agey weighty)




f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
    then Just (i + 1)
    else Nothing

h :: Integer -> Maybe String
h i = Just ("fuck" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)





