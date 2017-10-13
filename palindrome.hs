import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower, isLetter)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> do
      putStrLn "it is palindrome"
      exitSuccess
    False -> do
      putStrLn "Nope"
      exitSuccess

palindromeSentence :: String -> IO ()
palindromeSentence s = case (parsedLine == reverse parsedLine) of
                     True -> do
                       putStrLn "it is palindrome"
                       exitSuccess
                     False -> do
                       putStrLn "nope"
                       exitSuccess
                    where
                      parsedLine = foldr (++) [] $ words s

palindromeSentence' :: String -> IO ()
palindromeSentence' s = case (parsedLine == reverse parsedLine) of
                     True -> do
                       putStrLn "it is palindrome"
                       exitSuccess
                     False -> do
                       putStrLn "nope"
                       exitSuccess
                    where
                      parsedLine = filter isLetter s

