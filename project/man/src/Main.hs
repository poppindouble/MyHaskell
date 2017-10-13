module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
newtype WordList = WordList [String]
  deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter f aw)
    where
      f :: String -> Bool
      f s = (length s) < maxWordLength && (length s) > minWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just a) = a

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered guessed) c = Puzzle word newDiscovered (c : guessed)
  where
    newDiscovered = zipWith (f c) word discovered
    f :: Char -> Char -> Maybe Char -> Maybe Char
    f guessedChar wordChar discoveredMaybeChar = case guessedChar == wordChar of
    	                                           True -> (Just wordChar)
    	                                           False -> discoveredMaybeChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "you already guessed that"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "this char is not in the word"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word _ guessed) =
  if (length guessed) > 50 then
    do
      putStrLn $ "you lose, the word is " ++ word
      exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ discovered _) =
  if all isJust discovered then
    do
      putStrLn "you win"
      exitSuccess
  else
  	return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "your guess must be a single character"

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s [Nothing | x <- s] []

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
