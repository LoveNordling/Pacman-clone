module Core.GameEngine
        ( step, handleKeyEvents, fps, testSuite ) where

import Control.DeepSeq
import Test.HUnit hiding (State)
import Test.Hspec --(it, shouldThrow, anyException)
import Data.Array
import Control.Exception-- (evaluate)
import Graphics.Gloss.Interface.Pure.Game
import qualified Core.Board.Actor as Actor hiding (testSuite)
import qualified Core.Board.Level as Level hiding (testSuite)
import qualified Core.Board.Board as Board hiding (testSuite)
import qualified Core.Board.Tile as Tile
import qualified Core.Extras.Sprite as Sprite
import qualified Core.Extras.Resources as Resources
import qualified Core.AI as AI
import Core.Extras.Common
import Core.Board.GameState hiding (testSuite)

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
step :: Float -> GameState -> GameState
step _ state@(Splash _ _) = state
step _ state              = moveActors (setMovement state)

{- handleKeyEvents e g
   PRE:           True
   POST:          g updated based on e, if e is an arrow key event.
   SIDE EFFECTS:  None
   EXAMPLES:      Calling handleKeyEvents with the input event EventKey SpecialKey KeyUp and a state sets the direction of the player character in state to (0,1).
-}
handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey (SpecialKey k) Down _ _) (State b s (Actor.Actors p c) t) = State b s (Actor.Actors (changePlayerMovement p k) c) t
handleKeyEvents (EventKey _ Down _ _) (Splash _ s) = s
handleKeyEvents _ state                            = state

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
moveActors :: GameState -> GameState
moveActors state@(State l s (Actor.Actors player ai) t)
  | levelGoal <= s = Splash "Level done!" (nextState state)
  | otherwise      =
    if newAI `caughtPlayer` newPlayer
      then Splash "Game Over!" initialState
      else State level score (Actor.Actors newPlayer newAI) t
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
setMovement :: GameState -> GameState
setMovement (State level s (Actor.Actors player ai) t) =
  let
    board       = Level.getBoard level
    (newAI, t') = spawnAI level (t + timeStep) ai
  in
    State level s (Actor.Actors (setPlayerMovement board player) (setAIMovements board player newAI)) t'
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
      | zero direction || null paths = changeAIDirection position (calculateAIMovement board xy position)
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
  | zero direction && zero nextDirection = player
  | zero (direction + nextDirection)     = Actor.createPlayer position nextDirection nextDirection sprites -- fixes 180 movement delay bug.
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
--test1, test2, test3, test4, test5, test6, test7 :: Test
--testSuite = TestList [ test1, test2, test3, test4, test5, test6, test7 ]
testSuite = undefined
-- step
-- Comparing computer and player positions after two steps to see if the function performs.
test1 =
  let
    (State l s (Actor.Actors p1 c1) _) = step 0 (nextState initialState)
    s1 = State l s (Actor.Actors (changePlayerMovement p1 KeyUp) c1) 0
    s2@(State _ _ (Actor.Actors p2 c2) _) = step 0 s1
    (cp1, cp2) = (Actor.position (head c1), Actor.position (head c2))
    (pp1, pp2) = (Actor.position p1, Actor.position (p2))
  in
    TestLabel "Step Test #1" . TestCase $ assertBool "" (cp1 /= cp2 && pp1 /= pp2)
-- changePlayerMovement
test2 =
  let p = Actor.directions $ changePlayerMovement (Actor.createPlayer (0,0) (0,0) (0,0) []) KeyUp
  in  TestLabel "Change Player Movement Test #1" . TestCase $ assertEqual "" (0,1) (snd p)
test3 =
  let p = Actor.directions $ changePlayerMovement (Actor.createPlayer (0,0) (0,0) (0,1) []) KeyBegin
  in  TestLabel "Change Player Movement Test #1" . TestCase $ assertEqual "" (0,1) (snd p)
-- moveActors and setMovement
-- test4 =
--   let
--     state1 = moveActors (setMovement (nextState initialState))
--     state2 = moveActors state1


-- Exception tests
-- Using HSpec because HUnit sucks
-- testSuiteExceptions =
--   it "Move actors and set movement exception test #1" $ do
--     let (State l _ _ _) = nextState initialState
--     let a = Actor.createPlayer (10,10) (0,0) (0,0) []
--     let s = State l 0 (Actor.Actors a []) 0
--     let m = do return (step 0 s)
--     m `shouldThrow` (errorCall "Index")
--     -- expectationFailure "Hej"
