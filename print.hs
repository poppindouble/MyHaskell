module Print where

appendString :: [Char] -> [Char] -> [Char]
appendString x y = do
  x ++ y

nThElement :: [Char] -> Int -> Char
nThElement x y = do
  x !! y

dropString :: [Char] -> Int -> String
dropString x y = do
  drop y x

letterIndex :: Int -> Char
letterIndex x = do
  "Curry is awesome" !! x

rvrs :: String -> String
rvrs x = do
  concat [awesome, " ", is, " ", curry]
  where
    curry = take 5 x
    is = take 2 $ drop 6 x
    awesome = take 7 $ drop 9 x
  
  