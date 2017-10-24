import Hs

equals :: Eq a => a -> a -> String -> IO ()
equals left right message = let result = if left == right
                                         then "SUCCESS"
                                         else "FAIL"
                            in putStrLn (message ++ " -> " ++ result)

theEmptyWorld :: World
theEmptyWorld = Hs.emptyWorld (Dimension 10 10)

newSnake :: Hs.Snake
newSnake = Hs.Snake (Id 0) North (Coordinate 5 5) [(Coordinate 5 6), (Coordinate 4 6)]

worldWithNewSnake :: World
worldWithNewSnake = theEmptyWorld {snakes = [newSnake]}

worldWithSnakes :: World
worldWithSnakes = theEmptyWorld {snakes = [newSnake, anotherSnake]}

anotherSnake :: Snake
anotherSnake = Hs.Snake (Id 1) North (Coordinate 0 0) []

snakeMovement :: IO ()
snakeMovement = do
  let steppedSnake = Hs.Snake (Id 0) North (Coordinate 5 4) [(Coordinate 5 5), (Coordinate 5 6)]
  let worldWithSteppedSnake = theEmptyWorld {snakes = [steppedSnake]}
  equals (updateWorld worldWithNewSnake Step) worldWithSteppedSnake "All snakes should move"

snakeTurning :: IO ()
snakeTurning = do
  let turnedSnake = Hs.Snake (Id 0) East (Coordinate 5 5) [(Coordinate 5 6), (Coordinate 4 6)]
  let worldWithTurnedSnake = theEmptyWorld {snakes = [turnedSnake, anotherSnake]}
  equals (updateWorld worldWithSnakes (TurnSnake (Id 0) East)) worldWithTurnedSnake "A snake should be turned east"

addingRemovingSnakes :: IO ()
addingRemovingSnakes = do
  equals (updateWorld theEmptyWorld (AddSnake newSnake)) worldWithNewSnake "NewSnake should be added to the world."
  equals (updateWorld worldWithNewSnake (AddSnake newSnake)) worldWithNewSnake "NewSnake should be added only with unused id."
  equals (updateWorld worldWithSnakes (RemoveSnake (snakeId anotherSnake))) worldWithNewSnake "NewSnake should be removed from the world."

main :: IO ()
main = do
  snakeMovement
  snakeTurning
  addingRemovingSnakes
