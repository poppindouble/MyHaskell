module Main where

import Lib
import Control.Monad.State

{-
  Maze will always start at 0,0
  G is the goal
  The algorithm is to find the goal
  Also we need to mark the bad path with -
  and the right path with +
-}

type Maze = [String]
data Direction = North | West | South | East | Stay deriving (Eq, Show)
data Explorer = Explorer Int Int [Direction]
type GameState = (Explorer, Maze)

initMaze :: Maze
initMaze = [
    ".########" ,
    ".........#" ,
    "#.####..#" ,
    "#.#######" ,
    "...#....." ,
    "##...####" ,
    "#.G######"
  ]

initExplorer :: Explorer
initExplorer = Explorer 0 0 []

initGameState :: GameState
initGameState = (initExplorer, initMaze)

-- This is the core algorithm
walk :: State GameState ()
walk = do
  (explorer@(Explorer x y directionHistory), maze) <- get
  if foundTheGold x y maze
    then return ()
    else do
      let newMaze = markMaze explorer '+' maze
      let newExplorer@(Explorer _ _ directionHistory) = explore explorer maze
      if length directionHistory == 1 && last directionHistory == Stay
        then do
          return ()
        else do
          if last directionHistory == Stay
            then do
              let newMaze = markMaze explorer '-' maze
              let newExplorer = traceBack explorer
              put(newExplorer, newMaze)
              walk
            else do
              put(newExplorer, newMaze)
              walk

traceBack :: Explorer -> Explorer
traceBack (Explorer x y history)
  | last history == East = Explorer x (y - 1) (take (length history - 1) history)
  | last history == North = Explorer (x + 1) y (take (length history - 1) history)
  | last history == West = Explorer x (y + 1) (take (length history - 1) history)
  | last history == South = Explorer (x - 1) y (take (length history - 1) history)

explore :: Explorer -> Maze -> Explorer
explore (Explorer x y directionHistory) maze
  | isWalkable x (y + 1) maze = Explorer x (y + 1) (directionHistory ++ [East])
  | isWalkable (x - 1) y maze = Explorer (x - 1) y (directionHistory ++ [North])
  | isWalkable x (y - 1) maze = Explorer x (y - 1) (directionHistory ++ [West])
  | isWalkable (x + 1) y maze = Explorer (x + 1) y (directionHistory ++ [South])
  | otherwise = Explorer x y (directionHistory ++ [Stay])

isWalkable :: Int -> Int -> Maze -> Bool
isWalkable x y maze = isInsideMaze x y maze && (maze !! x !! y == '.' || maze !! x !! y == 'G')

isInsideMaze :: Int -> Int -> Maze -> Bool
isInsideMaze x y maze
  | x < 0 || x >= (length maze) = False
  | y < 0 || y >= (length $ maze !! 0) = False
  | otherwise = True

foundTheGold :: Int -> Int -> Maze -> Bool
foundTheGold x y maze = isInsideMaze x y maze && maze !! x !! y == 'G'

markMaze :: Explorer -> Char -> Maze -> Maze
markMaze (Explorer x y _) char maze = take x maze ++ [(changeNthCharInString y char (maze !! x))] ++ drop (x + 1) maze

changeNthCharInString :: Int -> Char -> String -> String
changeNthCharInString y c xs = take y xs ++ [c] ++ drop (y + 1) xs

main :: IO ()
main = do
  let (Explorer x y direction, maze) = execState walk initGameState
  mapM_ putStrLn maze
