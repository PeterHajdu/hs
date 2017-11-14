module Main where

import System.IO
import Hs

data Colour =
    Red
  | White
  | Green
  | Blue deriving (Eq)

setColour :: Colour -> IO ()
setColour c = do
  let code = case c of
               Red -> "31"
               White -> "97"
               Green -> "32"
               Blue -> "34"
  putStr $ "\ESC[" ++ code ++ "m"

hideCursor :: IO ()
hideCursor = putStr "\ESC[?25l"

showCursor :: IO ()
showCursor = putStr "\ESC[?25h"

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

tearDownTerminal :: IO ()
tearDownTerminal = do
  clearScreen
  showCursor
  setColour White

setUpTerminal :: IO ()
setUpTerminal = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  hideCursor

moveCursor :: Coordinate -> IO ()
moveCursor (Coordinate x' y') = putStr $ "\ESC[" ++ (show y') ++ ";" ++ (show x') ++ "H"

printSnake :: Snake -> IO ()
printSnake (Snake _ heading' h t) = do
  let headChar = case heading' of
                   West -> "<"
                   East -> ">"
                   North -> "^"
                   South -> "v"
  moveCursor h
  setColour Green
  putStr headChar
  setColour Blue
  mapM_ (\c -> (moveCursor c >> putStr "O")) t

printSnakes :: World -> IO ()
printSnakes world = let ss = snakes world
                    in mapM_ printSnake ss

printApple :: Apple -> IO ()
printApple (Apple pos) = do
  moveCursor pos
  setColour Red
  putStr "@"

printApples :: World -> IO ()
printApples world = mapM_ printApple (apples world)

printBorder :: World -> IO ()
printBorder (World (Dimension w h) _ _) = do
  let horizontal = [0..w]
  let vertical = [0..h]
  let upperBorder = map (\x' -> Coordinate x' 0) horizontal
  let lowerBorder = map (\x' -> Coordinate x' h) horizontal
  let leftBorder = map (\y' -> Coordinate 0 y') vertical
  let rightBorder = map (\y' -> Coordinate w y') vertical

  setColour White
  mapM_ (\c -> moveCursor c >> putStr "+" ) (upperBorder ++ lowerBorder ++ leftBorder ++ rightBorder)


printWorld :: World -> IO ()
printWorld world = do
  clearScreen
  printSnakes world
  printApples world
  printBorder world

data UserInput =
    Quit
  | Turn Direction
  | Unknown deriving (Eq)

charToInput :: Char -> UserInput
charToInput c = case c of
                  'q' -> Quit
                  'h' -> Turn West
                  'l' -> Turn East
                  'j' -> Turn South
                  'k' -> Turn North
                  _ -> Unknown

getInput :: IO UserInput
getInput =  charToInput <$> getChar

loop :: Hs.World -> IO ()
loop world = do
  printWorld world
  input <- getInput
  case input of
    Quit -> pure ()
    (Turn direction) -> do
      let newWorld = updateWorld (updateWorld world (TurnSnake (Id 0) direction)) Step
      loop newWorld
    Unknown -> loop (updateWorld world Step)

main :: IO ()
main = do
  setUpTerminal
  let snakes' = [(Snake (Id 0) East (Coordinate 10 5) [(Coordinate 9 5), (Coordinate 8 5), (Coordinate 7 5), (Coordinate 6 5)])]
  let apples' = [(Apple (Coordinate 5 5)), (Apple (Coordinate 15 7))]
  loop (World (Dimension 20 20) snakes' apples')
  tearDownTerminal
