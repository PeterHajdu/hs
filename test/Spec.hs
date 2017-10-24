import Hs

equals :: Eq a => a -> a -> String -> IO ()
equals left right message = let result = if left == right
                                         then "SUCCESS"
                                         else "FAIL"
                            in putStrLn (message ++ " -> " ++ result)

main :: IO ()
main = do
  let theEmptyWorld = Hs.emptyWorld (Dimension 10 10)
  let newSnake = Hs.Snake (Id 0) North (Coordinate 5 5) [(Coordinate 5 6), (Coordinate 4 6)]
  let turnedSnake = Hs.Snake (Id 0) East (Coordinate 5 5) [(Coordinate 5 6), (Coordinate 4 6)]
  let anotherSnake = Hs.Snake (Id 1) North (Coordinate 0 0) []
  let steppedSnake = Hs.Snake (Id 0) North (Coordinate 5 4) [(Coordinate 5 5), (Coordinate 5 6)]
  let worldWithNewSnake = theEmptyWorld {snakes = [newSnake]}
  let worldWithSnakes = theEmptyWorld {snakes = [newSnake, anotherSnake]}
  let worldWithTurnedSnake = theEmptyWorld {snakes = [turnedSnake, anotherSnake]}
  let worldWithSteppedSnake = theEmptyWorld {snakes = [steppedSnake]}

  equals (updateWorld theEmptyWorld (AddSnake newSnake)) worldWithNewSnake "NewSnake should be added to the world."
  equals (updateWorld worldWithNewSnake (AddSnake newSnake)) worldWithNewSnake "NewSnake should be added only with unused id."
  equals (updateWorld worldWithSnakes (RemoveSnake (snakeId anotherSnake))) worldWithNewSnake "NewSnake should be removed from the world."
  equals (updateWorld worldWithSnakes (TurnSnake (Id 0) East)) worldWithTurnedSnake "A snake should be turned east"
  equals (updateWorld worldWithNewSnake Step) worldWithSteppedSnake "All snakes should move"
