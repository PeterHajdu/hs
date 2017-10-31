import Hs
import Test.Hspec

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


snakeMovement :: Spec
snakeMovement = describe "world step" $ do
  describe "snakes move towards their heading" $ do
    it "north" $ do
      checkSnakeMovement
        (Hs.Snake (Id 0) North (Coordinate 5 4) [(Coordinate 5 5), (Coordinate 5 6)])
        (Hs.Snake (Id 0) North (Coordinate 5 3) [(Coordinate 5 4), (Coordinate 5 5)])

    it "west" $ do
      checkSnakeMovement
        (Hs.Snake (Id 0) West (Coordinate 5 4) [(Coordinate 5 5), (Coordinate 5 6)])
        (Hs.Snake (Id 0) West (Coordinate 4 4) [(Coordinate 5 4), (Coordinate 5 5)])

    it "east" $ do
      checkSnakeMovement
        (Hs.Snake (Id 0) East (Coordinate 5 4) [(Coordinate 5 5), (Coordinate 5 6)])
        (Hs.Snake (Id 0) East (Coordinate 6 4) [(Coordinate 5 4), (Coordinate 5 5)])

    it "south" $ do
      checkSnakeMovement
        (Hs.Snake (Id 0) South (Coordinate 5 4) [(Coordinate 4 4)])
        (Hs.Snake (Id 0) South (Coordinate 5 5) [(Coordinate 5 4)])

  where checkSnakeMovement oldSnake snakeAfterStep = do
          let oldWorld = theEmptyWorld {snakes = [oldSnake]}
          let newWorld = theEmptyWorld {snakes = [snakeAfterStep]}
          (updateWorld oldWorld Step) `shouldBe` newWorld

snakeTurning :: Spec
snakeTurning =
  describe "snake turning" $ do
    it "A snake should be turned east" $ do
      let turnedSnake = Hs.Snake (Id 0) East (Coordinate 5 5) [(Coordinate 5 6), (Coordinate 4 6)]
      let worldWithTurnedSnake = theEmptyWorld {snakes = [turnedSnake, anotherSnake]}
      (updateWorld worldWithSnakes (TurnSnake (Id 0) East)) `shouldBe` worldWithTurnedSnake

addingRemovingSnakes :: Spec
addingRemovingSnakes =
  describe "snake addition and removal to/from the world" $ do
    it "adds a snake to the world" $ do
      (updateWorld theEmptyWorld (AddSnake newSnake)) `shouldBe` worldWithNewSnake

    it "adds a snake only with unused id" $ do
      (updateWorld worldWithNewSnake (AddSnake newSnake)) `shouldBe` worldWithNewSnake

    it "removes the snake with the given id" $ do
      (updateWorld worldWithSnakes (RemoveSnake (snakeId anotherSnake))) `shouldBe` worldWithNewSnake

main :: IO ()
main = hspec $ do
  snakeTurning
  addingRemovingSnakes
  snakeMovement
