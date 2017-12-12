module Main where

import System.IO
import Hs
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Network.Socket
import System.Random

data Colour =
    Red
  | White
  | Green
  | Blue deriving (Eq)

setColour :: Colour -> String
setColour c =
  let code = case c of
               Red -> "31"
               White -> "97"
               Green -> "32"
               Blue -> "34"
  in "\ESC[" ++ code ++ "m"

hideCursor :: String
hideCursor = "\ESC[?25l"

showCursor :: String
showCursor = "\ESC[?25h"

clearScreen :: String
clearScreen = "\ESC[2J"

tearDownTerminal :: IO ()
tearDownTerminal = do
  putStr $ clearScreen ++ showCursor ++ (setColour White)

setUpTerminal :: IO ()
setUpTerminal = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  putStr hideCursor

moveCursor :: Coordinate -> String
moveCursor (Coordinate x' y') = "\ESC[" ++ (show y') ++ ";" ++ (show x') ++ "H"

printSnake :: Snake -> String
printSnake (Snake _ heading' h t) = do
  let headChar = case heading' of
                   West -> "<"
                   East -> ">"
                   North -> "^"
                   South -> "v"
  (moveCursor h) ++ (setColour Green) ++ headChar ++ (setColour Blue) ++ (foldMap (\c -> ((moveCursor c) ++ "O")) t)

printSnakes :: World -> String
printSnakes world = let ss = snakes world
                    in foldMap printSnake ss

printApple :: Apple -> String
printApple (Apple pos) = (moveCursor pos) ++ (setColour Red) ++ "@"

printApples :: World -> String
printApples world = foldMap printApple (apples world)

printBorder :: World -> String
printBorder (World (Dimension w h) _ _) =
  let horizontal = [0..w]
      vertical = [0..h]
      upperBorder = map (\x' -> Coordinate x' 0) horizontal
      lowerBorder = map (\x' -> Coordinate x' h) horizontal
      leftBorder = map (\y' -> Coordinate 0 y') vertical
      rightBorder = map (\y' -> Coordinate w y') vertical
  in (setColour White) ++ (foldMap (\c -> (moveCursor c) ++ "+" ) (upperBorder ++ lowerBorder ++ leftBorder ++ rightBorder))


printWorld :: World -> WorldChan -> IO ()
printWorld world worldChan = do
  let newWorld = clearScreen ++ (printSnakes world) ++ (printApples world) ++ (printBorder world)
  atomically $ writeTChan worldChan newWorld
  putStr $ newWorld

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

type EventChan = TChan Event
type WorldChan = TChan String

worldUpdate :: EventChan -> WorldChan -> Hs.World -> IO ()
worldUpdate chan worldChan world = do
  event <- atomically $ readTChan chan
  let newWorld = updateWorld world event
  if event == Step then (printWorld newWorld worldChan) else pure ()
  worldUpdate chan worldChan newWorld

stepSender :: EventChan -> IO ()
stepSender chan = forever $ do
  atomically $ writeTChan chan Step
  threadDelay 500000

inputSender :: EventChan -> IO ()
inputSender chan = do
  input <- getInput
  case input of
    Quit -> pure ()
    (Turn direction) -> do
      atomically $ writeTChan chan (TurnSnake (Id 0) direction)
      inputSender chan
    Unknown -> inputSender chan

clientLoop :: Int -> EventChan -> Socket -> IO ()
clientLoop index chan sock = forever $ do
  msg <- recv sock 1
  case (charToInput (head msg)) of
    (Turn direction) -> do
      atomically $ writeTChan chan (TurnSnake (Id index) direction)
    _ -> pure ()

clientUpdateLoop :: WorldChan -> Socket -> IO ()
clientUpdateLoop worldChan socket = forever $ do
  world <- atomically $ readTChan worldChan
  send socket world

clientMain :: Int -> EventChan -> WorldChan -> (Socket, SockAddr) -> IO ()
clientMain clientIndex chan worldChan (sock, _) = do
  atomically $ writeTChan chan (AddSnake (Snake (Id clientIndex) South (Coordinate (clientIndex + 1) (clientIndex+1)) []))
  _ <- forkIO $ clientUpdateLoop worldChan sock
  clientLoop clientIndex chan sock

listeningLoop :: Int -> EventChan -> WorldChan -> Socket -> IO ()
listeningLoop clientCount chan worldChan sock = do
  conn <- accept sock
  newchan <- atomically $ dupTChan worldChan
  _ <- forkIO $ clientMain clientCount chan newchan conn
  listeningLoop (clientCount + 1) chan worldChan sock

networkThread :: EventChan -> WorldChan -> IO ()
networkThread chan worldChan = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  listeningLoop 0 chan worldChan sock

appleSpawner :: Dimension -> StdGen -> EventChan -> IO ()
appleSpawner dim@(Dimension w h) gen chan = do
  let (x, newGen) = randomR (0, w) gen
  let (y, newGen') = randomR (0, h) newGen
  atomically $ writeTChan chan (AddApple (Apple (Coordinate x y)))
  threadDelay 1000000
  appleSpawner dim newGen' chan

start :: Hs.World -> IO ()
start world = do
  chan <- newTChanIO
  worldChan <- atomically $ newBroadcastTChan
  _ <- forkIO $ worldUpdate chan worldChan world
  _ <- forkIO $ stepSender chan
  _ <- forkIO $ networkThread chan worldChan

  stdGen <- newStdGen
  _ <- forkIO $ appleSpawner (dimension world) stdGen chan
  inputSender chan

main :: IO ()
main = do
  setUpTerminal
  let snakes' = [(Snake (Id 0) East (Coordinate 10 5) [(Coordinate 9 5), (Coordinate 8 5), (Coordinate 7 5), (Coordinate 6 5)])]
  let apples' = [ (Apple (Coordinate 5 5)), (Apple (Coordinate 15 7)), (Apple (Coordinate 14 7)), (Apple (Coordinate 13 7)), (Apple (Coordinate 12 7)), (Apple (Coordinate 11 7)), (Apple (Coordinate 10 10)), (Apple (Coordinate 9 9)), (Apple (Coordinate 16 8)) ]
  start (World (Dimension 40 40) [] apples')
  tearDownTerminal
