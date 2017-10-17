import Hs

equals :: Eq a => a -> a -> String -> IO ()
equals left right message = let result = if left == right
                                         then "SUCCESS"
                                         else "FAIL"
                            in putStrLn (message ++ " -> " ++ result)

main :: IO ()
main = do
  let theEmptyWorld = Hs.emptyWorld (Dimension 10 10)
  let newSnake = Hs.Snake (Id 0) North (Coordinate 0 0) []
  let anotherSnake = Hs.Snake (Id 1) North (Coordinate 0 0) []
  let worldWithNewSnake = theEmptyWorld {snakes = [newSnake]}
  let worldWithSnakes = theEmptyWorld {snakes = [newSnake, anotherSnake]}
  equals (updateWorld theEmptyWorld (AddSnake newSnake)) worldWithNewSnake "NewSnake should be added to the world."
  equals (updateWorld worldWithNewSnake (AddSnake newSnake)) worldWithNewSnake "NewSnake should be added only with unused id."
  equals (updateWorld worldWithSnakes (RemoveSnake (snakeId anotherSnake))) worldWithNewSnake "NewSnake should be removed from the world."
