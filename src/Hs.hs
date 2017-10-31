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
  , shead:: Coordinate
  , stail:: [Coordinate]
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

updateWorld world (TurnSnake sid direction) = let oldSnakes = snakes world
                                                  newSnakes = modifyElem (\snake -> (snakeId snake) == sid) (\snake -> snake { heading = direction}) oldSnakes
                                              in world {snakes = newSnakes}

updateWorld world (RemoveSnake sid) = let oldSnakes = snakes world
                                          newSnakes = filter (\snake -> (snakeId snake) /= sid) oldSnakes
                                      in world {snakes = newSnakes}

updateWorld world Step = moveSnakes world


moveSnakes :: World -> World
moveSnakes world = let oldSnakes = snakes world
                       newSnakes = map (moveOneSnake world) oldSnakes
                       newSnakeHeads = (map shead newSnakes) :: [Coordinate]
                       notEaten (Apple pos) = not $ elem pos newSnakeHeads
                       leftOverApples = filter notEaten (apples world)
                   in world {snakes = newSnakes, apples = leftOverApples}

moveOneSnake :: World -> Snake -> Snake
moveOneSnake world snake = let newHead = calcNewHead snake
                               newTail = if (elem (Apple newHead) (apples world))
                                         then (shead snake) : (stail snake)
                                         else init $ (shead snake) : (stail snake)
                           in snake {shead = newHead, stail = newTail}

calcNewHead :: Snake -> Coordinate
calcNewHead (Snake _ currentDirection (Coordinate x' y')  _) = case currentDirection of
                                                      North -> Coordinate x' (y' - 1)
                                                      West -> Coordinate (x' - 1) y'
                                                      East -> Coordinate (x' + 1) y'
                                                      South -> Coordinate x' (y' + 1)



modifyElem :: Functor f => (a -> Bool )-> (a -> a)-> f a -> f a
modifyElem predicate modifier = fmap (\e -> if predicate e then modifier e else e)
