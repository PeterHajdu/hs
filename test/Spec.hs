import Hs
import Test.Hspec

theEmptyWorld :: World
theEmptyWorld = Hs.emptyWorld (Dimension 10 20)

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
  describe "collision with wall" $ do
    let dim = dimension theEmptyWorld
    it "north wall" $ checkWallCollision theEmptyWorld (Coordinate 3 0) North
    it "west wall" $ checkWallCollision theEmptyWorld (Coordinate 0 2) West
    it "east wall" $ checkWallCollision theEmptyWorld (Coordinate ((width dim) - 1) 2) East
    it "south wall" $ checkWallCollision theEmptyWorld (Coordinate 2 ((height dim) - 1)) South

  describe "colliding snakes" $ do
    it "dies if it's head hits another snake" $ do
      let oneStepBeforeDeath = theEmptyWorld {snakes =
          [ newSnake
          , Hs.Snake (Id 12) East (Coordinate 4 5) []
          ]}
      let snakesAfterDeath = snakes (updateWorld oneStepBeforeDeath Step)
      let newSnakeAfterStep = Hs.Snake (Id 0) North (Coordinate 5 4) [(Coordinate 5 5), (Coordinate 5 6)]
      snakesAfterDeath `shouldBe` [newSnakeAfterStep]

    it "both snakes die on head collision" $ do
      let oneStepBeforeDeath = theEmptyWorld {snakes =
          [ newSnake
          , Hs.Snake (Id 12) East (Coordinate 4 4) []
          ]}
      let snakesAfterDeath = snakes (updateWorld oneStepBeforeDeath Step)
      snakesAfterDeath `shouldBe` []

  describe "snake that eats" $ do
    it "grows" $ do
      let apple = Apple (Coordinate 5 4)

      let snake = Snake (Id 0) North (Coordinate 5 5) []
      let biggerSnake = Snake (Id 0) North (Coordinate 5 4) [Coordinate 5 5]

      checkEating
        (Hs.World (Dimension 10 10) [snake] [apple])
        (Hs.World (Dimension 10 10) [biggerSnake] [])

  describe "snakes move towards their heading" $ do
    it "does not grow while moving" $ do
      checkSnakeMovement
        (Hs.Snake (Id 0) North (Coordinate 5 4) [])
        (Hs.Snake (Id 0) North (Coordinate 5 3) [])

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

  where checkEating oldWorld newWorld = do
          (updateWorld oldWorld Step) `shouldBe` newWorld

        checkSnakeMovement oldSnake snakeAfterStep = do
          let oldWorld = theEmptyWorld {snakes = [oldSnake]}
          let newWorld = theEmptyWorld {snakes = [snakeAfterStep]}
          (updateWorld oldWorld Step) `shouldBe` newWorld

        checkWallCollision w snakeHead snakeHeading = do
          let oneStepBeforeDeath = w {snakes =
              [Hs.Snake (Id 12) snakeHeading snakeHead []]}
          let snakesAfterDeath = snakes (updateWorld oneStepBeforeDeath Step)
          snakesAfterDeath `shouldBe` []

snakeTurning :: Spec
snakeTurning =
  describe "snake turning" $ do
    it "A snake should be turned east" $ do
      let turnedSnake = Hs.Snake (Id 0) East (Coordinate 5 5) [(Coordinate 5 6), (Coordinate 4 6)]
      let worldWithTurnedSnake = theEmptyWorld {snakes = [turnedSnake, anotherSnake]}
      (updateWorld worldWithSnakes (TurnSnake (Id 0) East)) `shouldBe` worldWithTurnedSnake

    describe "tailless snake" $ do
      it "should turn regardless of heading" $ do
        let snake = Hs.Snake (Id 0) North (Coordinate 5 5) []
        let worldWithSnake = theEmptyWorld {snakes = [snake]}
        snakes (updateWorld worldWithSnake (TurnSnake (Id 0) South)) `shouldBe` [snake {heading = South}]

    describe "snake with tail can not turn to itself" $ do
      it "south" $ do
        let snake = Hs.Snake (Id 0) North (Coordinate 5 5) [(Coordinate 5 6)]
        let worldWithSnake = theEmptyWorld {snakes = [snake]}
        snakes (updateWorld worldWithSnake (TurnSnake (Id 0) South)) `shouldBe` [snake]

      it "north" $ do
        let snake = Hs.Snake (Id 0) South (Coordinate 5 6) [(Coordinate 5 5)]
        let worldWithSnake = theEmptyWorld {snakes = [snake]}
        snakes (updateWorld worldWithSnake (TurnSnake (Id 0) North)) `shouldBe` [snake]

      it "east" $ do
        let snake = Hs.Snake (Id 0) West (Coordinate 5 6) [(Coordinate 6 6)]
        let worldWithSnake = theEmptyWorld {snakes = [snake]}
        snakes (updateWorld worldWithSnake (TurnSnake (Id 0) East)) `shouldBe` [snake]

      it "west" $ do
        let snake = Hs.Snake (Id 0) East (Coordinate 7 6) [(Coordinate 6 6)]
        let worldWithSnake = theEmptyWorld {snakes = [snake]}
        snakes (updateWorld worldWithSnake (TurnSnake (Id 0) West)) `shouldBe` [snake]


addingRemovingSnakes :: Spec
addingRemovingSnakes =
  describe "snake addition and removal to/from the world" $ do
    it "adds a snake to the world" $ do
      (updateWorld theEmptyWorld (AddSnake newSnake)) `shouldBe` worldWithNewSnake

    it "adds a snake only with unused id" $ do
      (updateWorld worldWithNewSnake (AddSnake newSnake)) `shouldBe` worldWithNewSnake

    it "removes the snake with the given id" $ do
      (updateWorld worldWithSnakes (RemoveSnake (snakeId anotherSnake))) `shouldBe` worldWithNewSnake

addingApples :: Spec
addingApples =
  describe "adding apples" $ do
    it "adds a new apple to the world" $ do
      let newApple = Apple (Coordinate 3 2)
      let worldWithApple = updateWorld theEmptyWorld (AddApple newApple)
      (apples worldWithApple) `shouldBe` [newApple]

main :: IO ()
main = hspec $ do
  snakeTurning
  addingRemovingSnakes
  addingApples
  snakeMovement
