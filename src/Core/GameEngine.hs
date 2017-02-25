module Core.GameEngine
        ( step, handleKeyEvents, fps, testSuite, testSuiteExceptions ) where

-- Modules for testing
import Test.Hspec
import Test.HUnit hiding (State)
import Control.Exception (evaluate)

-- External modules
import Data.Array
import Graphics.Gloss.Interface.Pure.Game

-- Internal modules
import qualified Core.Board.Actor as Actor hiding (testSuite)
import qualified Core.Board.Level as Level hiding (testSuite)
import qualified Core.Board.Board as Board hiding (testSuite)
import qualified Core.Board.Tile as Tile
import qualified Core.Extras.Sprite as Sprite
import qualified Core.AI as AI
import qualified Core.Extras.Common as Common
import qualified Core.Board.GameState as GameState hiding (testSuite)

{-# ANN module "HLint: Ignore Use fmap" #-}

-- The FPS of the game
fps :: Int
fps = 60

--Maximum amount of AIs
maxAI :: Int
maxAI = 5

--The time it takes for a ghost to spawn
spawnTime :: Float
spawnTime = 5.0

--The amount of time between each frame (Used for Ghost spawning)
timeStep :: Float
timeStep = 1 / (fromIntegral fps)

-- The player and AI speeds
playerSpeed, aiSpeed :: (Float, Float)
playerSpeed = actorSpeed 3.8 (fromIntegral fps)
aiSpeed     = actorSpeed 2 (fromIntegral fps)

{- actorSpeed s f
   PRE:           f > 0
   POST:          A speed as the ratio of s and f
   SIDE EFFECTS:  None
   EXAMPLES:      actorSpeed 5 10   == (0.5, 0.5)
                  actorSpeed 1 (-5) == (-0.2, -0.2)
-}
actorSpeed :: Float -> Float -> (Float, Float)
actorSpeed speed fps = (speed / fps, speed / fps)

{- step s g
   PRE:           True
   POST:          If g is a splash screen, then g. Otherwise the state of the game updated.
   SIDE EFFECTS:  None
   EXAMPLES:      Calling step on 1.0 and a state that is not a splash page gives an updated state.
-}
step :: Float -> GameState.GameState -> GameState.GameState
step _ state@(GameState.Splash _ _) = state
step _ state                        = moveActors (setMovement state)

{- handleKeyEvents e g
   PRE:           True
   POST:          g updated based on e, if e is an arrow key event.
   SIDE EFFECTS:  None
   EXAMPLES:      Calling handleKeyEvents with the input event EventKey SpecialKey KeyUp and a state sets the direction of the player character in state to (0,1).
-}
handleKeyEvents :: Event -> GameState.GameState -> GameState.GameState
handleKeyEvents (EventKey (SpecialKey k) Down _ _) (GameState.State b s (Actor.Actors p c) t) =
  GameState.State b s (Actor.Actors (changePlayerMovement p k) c) t
handleKeyEvents (EventKey _ Down _ _) (GameState.Splash _ state) = state
handleKeyEvents _ state = state

{- changePlayerMovement p
   PRE:           True
   POST:          p with updated desired direction for player
   SIDE EFFECTS:  None
   EXAMPLES:      changePlayerMovement (Actor.createPlayer (0,0) (0,0) (0,0) []) KeyUp == Actor.createPlayer (0.0,0.0) (0.0,0.0) (0.0,1.0) Sprite.player
                  changePlayerMovement (Actor.createPlayer (0,0) (0,0) (0,0) []) KeyBegin == Actor.createPlayer (0,0) (0,0) (0,0) Sprite.player
-}
changePlayerMovement :: Actor.Actor -> SpecialKey -> Actor.Actor
changePlayerMovement player k =
  let
    sprites                    = Sprite.player
    position                   = Actor.position player
    (direction, nextDirection) = Actor.directions player
    next = case k of
            KeyUp    -> (0, 1)
            KeyLeft  -> (-1,0)
            KeyDown  -> (0,-1)
            KeyRight -> (1, 0)
            _        -> nextDirection
  in
    Actor.createPlayer position direction next sprites

{- moveActors s
   PRE:           True
   POST:          s with its actors position updated.
   SIDE EFFECTS:  None
   EXAMPLES:      moveActors (setMovement (nextState initialState)) == a new state with the position of the states actors updated one step according to their direction.
                  moveActors (nextState initialState) == (nextState initialState)
-}
moveActors :: GameState.GameState -> GameState.GameState
moveActors state@(GameState.State l s (Actor.Actors player ai) t)
  | levelGoal <= s = GameState.Splash "Level done!" (GameState.nextState state)
  | otherwise      =
    if newAI `caughtPlayer` newPlayer
      then GameState.Splash "Game Over!" GameState.initialState
      else GameState.State level score (Actor.Actors newPlayer newAI) t
  where
    levelGoal      = Level.getGoal l
    newAI          = map (Actor.makeMove aiSpeed) ai
    newPlayer      = Actor.makeMove playerSpeed player
    (level, score) = case Level.checkForTreasure l (closestTile (Actor.position newPlayer)) of
                      Just l' -> (l', s + 1)
                      Nothing -> (l, s)
    {- caughtPlayer ai p
       PRE:           True
       POST:          True if any element in ai is on same position as p
       SIDE EFFECTS:  None
       VARIANT:       |ai|
       EXAMPLES:      caughtPlayer [(Actor.createAI (1,1) (0,0) [] [])] $ Actor.createPlayer (1,1) (0,0) (0,0) [] == True
    -}
    caughtPlayer :: [Actor.Actor] -> Actor.Actor -> Bool
    caughtPlayer []            _ = False
    caughtPlayer (ai:ais) player = (abs (x - a) < 0.5 && abs (y - b) < 0.5) || ais `caughtPlayer` player
      where
        (a, b) = Actor.position ai
        (x, y) = Actor.position player
moveActors state  = state -- On game over!

{- setMovement s
   PRE:           True
   POST:          s with new movements for its entities
   SIDE EFFECTS:  None
   EXAMPLES:      setMovement (nextState initialState) == a state where the movements of the actors have been updated one step.
-}
setMovement :: GameState.GameState -> GameState.GameState
setMovement (GameState.State level s (Actor.Actors player ai) t) =
  let
    board       = Level.getBoard level
    (newAI, t') = spawnAI level (t + timeStep) ai
  in
    GameState.State level s (Actor.Actors (setPlayerMovement board player) (setAIMovements board player newAI)) t'
  where
    {- spawnAI l t c
       PRE:           spawn position of l must be valid coordinates for the board of l.
       POST:          new AIs and 0 if t > the spawn time of AI and length cs < maximum number of AIs, otherwise cs and t.
       SIDE EFFECTS:  None
       EXAMPLES:      Calling spawnAI on a level (with spawn position (5,5)), 15 and an empty list gives a tuple with the first component consisting of a list of just one AI, and the second component 0.
    -}
    spawnAI :: Level.Level -> Float -> [Actor.Actor] -> ([Actor.Actor], Float)
    spawnAI level t cs =
      let
        coords = Level.spawnPosition level
        nextAI = Actor.createAI coords (0,0) [] Sprite.ai
      in
        if t > spawnTime && length cs < maxAI
          then ((nextAI:cs), 0)
          else (cs, t)

{- setAIMovements b p c
   PRE:           Position of each element in c and p must be valid coordinates in b
   POST:          c where each element has new directions and shortest valid paths in b to position of p
   SIDE EFFECTS:  Prints error message to the screen if position of p and the positions of each element in c is not valid coordinates in b.
   EXAMPLES:      setAIMovements (Board.createBoard (Resources.levels !! 0)) (Actor.createPlayer (1,1) (0,0) (0,0) []) [(Actor.createAI (2,2) (0,0) [] [])] == An AI with paths [(1,2),(1,1)]
                  setAIMovements (Board.createBoard (Resources.levels !! 0)) (Actor.createPlayer (1,1) (0,0) (0,0) []) [] == []
-}
setAIMovements :: Board.Board -> Actor.Actor -> [Actor.Actor] -> [Actor.Actor]
setAIMovements _ _    [] = []
setAIMovements board p c = map (setAIMovement board (Actor.position p)) c
  where
    {- setAIMovement b xy c
       PRE:           xy and position of c must be valid coordinates in b
       POST:          c with new direction and a shortest path in b to xy
       SIDE EFFECTS:  Prints error message to the screen if xy or position of c is not a valid coordinates in b
       EXAMPLES:      setAIMovement (Board.createBoard (Resources.levels !! 0)) (1,1) (Actor.createAI (2,2) (0,0) [] []) == An AI with position (2,2), direction (-1,0) and path [(1,2), (1,1)]
       VARIANT:       |c|
    -}
    setAIMovement :: Board.Board -> (Float, Float) -> Actor.Actor -> Actor.Actor
    setAIMovement board xy c
      | Common.zero direction || null paths = changeAIDirection position (calculateAIMovement board xy position)
      | otherwise =
        if (hasReachedDestination aiSpeed position (head paths))
          then changeAIDirection position (calculateAIMovement board xy position)
          else Actor.createAI position direction paths Sprite.ai
      where
        position  = Actor.position c
        direction = Actor.direction c
        paths     = Actor.paths c
        {- calculateAIMovement b d s
           PRE:           d and s must be valid coordinates in b
           POST:          Shortest path in b from s to d
           SIDE EFFECTS:  Prints error message to the screen if d and s is not valid coordinates in b
           EXAMPLES:      calculateAIMovement (Board.createBoard (Resources.levels !! 0)) (1,1) (2,2)     == [(1,2),(1,1)]
           EXAMPLES:      calculateAIMovement (Board.createBoard (Resources.levels !! 0)) (999,999) (2,2) == [(2,2)]
                          calculateAIMovement (Board.createBoard (Resources.levels !! 0)) (88,88) (99,99) == an exception
        -}
        calculateAIMovement :: Board.Board -> (Float, Float) -> (Float, Float) -> [(Int, Int)]
        calculateAIMovement board destination start =
          let (d, s) = (closestTile destination, closestTile start)
          in  case (AI.aStar board d s) of
                [] -> [s]
                ps -> ps
        {- changeAIDirection p p'
           PRE:           p' must non-empty
           POST:          a computer with position based on p, direction based on p and first element of p', and p' as its paths
           SIDE EFFECTS:  Prints error message to the screen if p' is empty
           EXAMPLES:      changeAIDirection (1,1) [(1,2), (2,2)] == a computer with position (1,1), direction (0,1), paths [(1,2),(2,2)]
        -}
        changeAIDirection :: (Float, Float) -> [(Int, Int)] -> Actor.Actor
        changeAIDirection position path@((x', y'):_) = Actor.createAI position direction path Sprite.ai
          where direction = (fromIntegral x', fromIntegral y') - position

{- setPlayerMovement b a
   PRE:           Position of a must be a valid coordinates in b
   POST:          a if player is not in motion or if player is between tiles, otherwise a player with a valid direction
   SIDE EFFECTS:  Prints error message to the screen if position of a is not valid coorindates in b
   EXAMPLES:      setPlayerMovement (Board.createBoard (Resources.levels !! 0)) (Actor.createPlayer (1,1) (0,1) (1,0) Sprite.player) == (Actor.createPlayer (1,1) (1,0) (1,0) Sprite.player)
-}
setPlayerMovement :: Board.Board -> Actor.Actor -> Actor.Actor
setPlayerMovement board player
  | Common.zero direction && Common.zero nextDirection = player
  | Common.zero (direction + nextDirection) =
    Actor.createPlayer position nextDirection nextDirection sprites -- fixes 180 movement delay bug.
  | otherwise =
    if (hasReachedDestination playerSpeed position (closestTile position))
      then
        Actor.createPlayer position (changePlayerDirection board position direction nextDirection) nextDirection sprites
      else
        player
  where
    (position, (direction, nextDirection), sprites) = (Actor.position player, Actor.directions player, Sprite.player)
    {- setPlayerDirection b p d nd
       PRE:           p, p + d and p + nd must be valid coordinates in b
       POST:          nd if the approximation of p + nd will produce a valid move in b, d if the approximation of p + d is a valid move in b. Otherwise (0,0)
       SIDE EFFECTS:  Prints an error to the screen in p, or the sum of p and d or nd is not valid coordinates in b
       EXAMPLES:      changePlayerDirection (Board.createBoard (Resources.levels !! 0)) (1,1) (0,1) (1,0) == (1,0)
                      changePlayerDirection (Board.createBoard (Resources.levels !! 0)) (1,1) (1,0) (0,1) == (0,1)
    -}
    changePlayerDirection :: Board.Board -> (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
    changePlayerDirection board position direction nextDirection =
      let
        approxPosition = approximatePosition position (fst playerSpeed)
      in
        if isValidMove board (closestTile (approxPosition + nextDirection))
          then nextDirection
          else if isValidMove board (closestTile (approxPosition + direction))
            then direction
            else (0, 0)

{- closestTile p
   PRE:           True
   POST:          A nearest whole number position to p
   SIDE EFFECTS:  None
   EXAMPLES:      closestTile (1.43, 2.56) == (1,3)
-}
closestTile :: (Float, Float) -> (Int, Int)
closestTile (a, b) = (round a, round b)

{- isValidMove b p
   PRE:           b must have an element with index p
   POST:          True if element with key p in b is a valid tile to move to, otherwise False.
   SIDE EFFECTS:  Prints an error message to the screen if p exceeds the bounds of b
   EXAMPLES:      isValidMove (Board.createBoard (Resources.levels !! 0)) (0,0) == False
                  isValidMove (Board.createBoard (Resources.levels !! 0)) (1,2) == True
-}
isValidMove :: Board.Board -> (Int, Int) -> Bool
isValidMove board position =
  case (board ! position) of
    (Tile.Floor _ _) -> True
    _                -> False

{- approximatePosition p s
   PRE:           True
   POST:          p with components rounded if the distance between the rounded components and p is less than s, otherwise p
   SIDE EFFECTS:  None
   EXAMPLES:      approximatePosition (4.999999, 8.99999) (fst playerSpeed) == (5.0, 9.0)
                  approximatePosition (4.5, 8.5)          (fst playerSpeed) == (4.5, 8.5)
-}
approximatePosition :: (Float, Float) -> Float -> (Float, Float)
approximatePosition (x, y) speed =
  let
    tx = fromIntegral (round x)
    ty = fromIntegral (round y)
  in
    if abs (x - tx) < speed*2/3 && abs (y - ty) < speed*2/3
      then (tx, ty)
      else (x, y)

{- hasReachedDestination s p1 p2
   PRE:           True
   POST:          True if distance between the approximation of p1 and p2 is less than s, otherwise False
   SIDE EFFECTS:  None
   EXAMPLES:      hasReachedDestination playerSpeed (4.036667,4.036667) (4,4) == True
                  hasReachedDestination playerSpeed (6,6)               (4,4) == False
-}
hasReachedDestination :: (Float, Float) -> (Float, Float) -> (Int, Int) -> Bool
hasReachedDestination (speed,_) current (x, y) = (approximatePosition current speed) == (fromIntegral x, fromIntegral y)

-------------------------------------------
-- TEST CASES
-------------------------------------------
test1, test2, test3, test4, test5, test6, test7 :: Test
testSuite = TestList [ test1, test2, test3, test4, test5, test6, test7 ]
-- Setups
testBoard = Level.getBoard level
  where (Just (level,_)) = Level.setLevel 0

-- step, changePlayerMovement
test1 = -- Comparing computer and player positions after two steps to see if the function performs.
  let
    (GameState.State l _ (Actor.Actors p1 c1) _) = step 0 (GameState.nextState GameState.initialState)
    s1 = GameState.State l 0 (Actor.Actors (changePlayerMovement p1 KeyUp) c1) 0
    s2@(GameState.State _ _ (Actor.Actors p2 c2) _) = step 0 s1
    (cp1, cp2) = (Actor.position (head c1), Actor.position (head c2))
    (pp1, pp2) = (Actor.position p1, Actor.position (p2))
  in
    TestLabel "Step Test #1" . TestCase $ assertBool "Both AI and player should move" (cp1 /= cp2 && pp1 /= pp2)
-- changePlayerMovement
test2 = -- Testing direction change
  let p = Actor.directions $ changePlayerMovement (Actor.createPlayer (0,0) (0,0) (0,0) []) KeyUp
  in  TestLabel "Change Player Movement Test #1" . TestCase $ assertEqual "Should return new actor directions" (0,1) (snd p)
test3 =
  let p = Actor.directions $ changePlayerMovement (Actor.createPlayer (0,0) (0,0) (0,1) []) KeyBegin
  in  TestLabel "Change Player Movement Test #2" . TestCase $ assertEqual "Should return actor as is" (0,1) (snd p)
-- moveActors
test4 =
  let
    Just (l,_) = Level.setLevel 0
    p = Actor.createPlayer (1,1) (-1,0) (1,0) []
    (GameState.State _ _ (Actor.Actors p2 _) _) = moveActors (GameState.State l 0 (Actor.Actors p []) 0)
  in
    TestLabel "Move Actors Test #1" .
      TestCase $ assertBool "Actor position should change" ((Actor.position p) > (Actor.position p2))
-- setAIMovements
test5 =
  let
    player = Actor.createPlayer (3,3) (0,0) (0,0) []
    ai     = Actor.createAI (7,6) (0,0) [] []
    newAI  = head (setAIMovements testBoard player [ai])
  in
    TestLabel "Set AI Movements Test #1" .
      TestCase $ assertBool "AI direction should change" ((Actor.direction ai) /= (Actor.direction newAI))
-- setPlayerMovement
test6 =
  let
    a = Actor.createPlayer (1,1) (0,0) (0,0) Sprite.player
    n = setPlayerMovement testBoard a
  in
    TestLabel "Set Player Movement Test #1" .
      TestCase $ assertEqual "Should return the player as is" (a) (n)
test7 =
  let
    d = (0,1)
    a = Actor.createPlayer (1,1) (1,0) d Sprite.player
    n = setPlayerMovement testBoard a
  in
    TestLabel "Set Player Movement Test #1" .
      TestCase $ assertEqual "Should return the desired direction as its current direction" (d) (Actor.direction n)

-- EXCEPTION TESTS
-- Using HSpec because HUnit sucks
-------------------------------------------
testSuiteExceptions = do
  describe "Step Exception Tests" $ do
    it "Should throw exception Error in array index" $ do
      let (GameState.State level _ _ _) = GameState.nextState GameState.initialState
      let state = GameState.State level 0 (Actor.Actors (Actor.createPlayer (10,10) (0,0) (0,0) []) []) 0
      let (GameState.State badLevel _ _ _) = step 0 state
      -- we must pattern match here, otherwise we'll get an uncaught exception because of nested data types and lazy eval bullshit, I think. See HSpec source for their impl of shouldThrow (it returns an Either, but if the exception is nested within a data type it will still be a Right ...).
      evaluate (badLevel) `shouldThrow` (errorCall "Error in array index")
  describe "Set AI movements Exception Test" $ do
    it "Should throw exception Error in array index" $ do
      let player = Actor.createPlayer (1,1) (0,0) (0,0) []
      let someAI = Actor.createAI (10,10) (0,0) [] []
      -- Again, we need to actually retrieve the element that throws an exception (Right with Left still gives a Right...)
      let badAI  = head (setAIMovements testBoard player [someAI])
      evaluate (badAI) `shouldThrow` (errorCall "Error in array index")
