module Hs
    ( emptyWorld
    , Dimension(..)
    , Snake(..)
    , updateWorld
    , Id(..)
    , Coordinate(..)
    , Event(..)
    , World(..)
    , Direction(..)
    , Apple(..)
    ) where

data Dimension = Dimension
  { width:: Int
  , height:: Int
  } deriving (Show, Eq)

data Coordinate = Coordinate
  { x:: Int
  , y:: Int
  } deriving (Show, Eq)

newtype Id = Id Int deriving (Eq, Show)

data Snake = Snake
  { snakeId:: Id
  , heading:: Direction
  , head:: Coordinate
  , tail:: [Coordinate]
  } deriving (Show, Eq)

data Apple = Apple
  { position:: Coordinate
  } deriving (Show, Eq)

data World = World
  { dimension:: Dimension
  , snakes:: [Snake]
  , apples:: [Apple]
  } deriving (Show, Eq)

emptyWorld :: Dimension -> World
emptyWorld dim = World dim [] []

data Direction =
    North
  | South
  | West
  | East deriving (Show, Eq)

data Event =
    TurnSnake Id Direction
  | Step
  | AddSnake Snake
  | RemoveSnake Id deriving (Show)

updateWorld :: World -> Event -> World
updateWorld world (AddSnake snake) = let oldSnakes = snakes world
                                         newSnakeId = snakeId snake
                                         idAlreadyInUse = any (\s -> (snakeId s) == newSnakeId) oldSnakes
                                         newSnakes = if idAlreadyInUse then oldSnakes else snake:oldSnakes
                                     in world {snakes = newSnakes}
updateWorld world (RemoveSnake sid) = let oldSnakes = snakes world
                                          newSnakes = filter (\snake -> (snakeId snake) /= sid) oldSnakes
                                      in world {snakes = newSnakes}
updateWorld _ _ = undefined
