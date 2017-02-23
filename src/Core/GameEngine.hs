module Core.GameEngine (step, handleKeyEvents, fps) where

import Data.Array
import Graphics.Gloss.Interface.Pure.Game

import qualified Core.Board.Actor as Actor
import qualified Core.Board.Level as Level
import qualified Core.Board.Tile as Tile
import qualified Core.Extras.Resources as Resources
import Core.Board.GameState
import Core.Board.Board
import Core.Extras.Common
import Core.AI

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
timeStep = 1/(fromIntegral fps)

-- The player and AI speeds
playerSpeed, aiSpeed :: (Float, Float)
playerSpeed = actorSpeed 3.8 (fromIntegral fps)
aiSpeed     = actorSpeed 2 (fromIntegral fps)

{- actorSpeed s f
   PRE:       fps > 0
   POST:      A speed as the ratio of s and f
   EXAMPLES:  actorSpeed 5 10   == (0.5, 0.5)
              actorSpeed 1 (-5) == (-0.2, -0.2)
-}
actorSpeed :: Float -> Float -> (Float, Float)
actorSpeed speed fps = (speed / fps, speed / fps)

{- step s g
   PRE:       True
   POST:      g with updated state
   EXAMPLES:  step 1 state gives a
-}
step :: Float -> GameState -> GameState
step _ state@(Splash _ _) = state
step _ state = moveActors (setMovement state)

{- handleKeyEvents e g
   PRE:       True
   POST:      g with updated state if e is an event on arrow keys.
   EXAMPLES:  handleKeyEvents  ==
-}
handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey (SpecialKey k) Down _ _) (State b s (Actor.Actors p c) t) = State b s (Actor.Actors (changePlayerMovement p k) c) t
handleKeyEvents (EventKey _ Down _ _) (Splash _ s) = s
handleKeyEvents _ state = state

{- changePlayerMovement s
   PRE:       True
   POST:      s with updated desired direction for player.
   EXAMPLES:  changePlayerMovement ==
-}
changePlayerMovement :: Actor.Actor -> SpecialKey -> Actor.Actor
changePlayerMovement player k =
  let
    sprites  = Actor.sprites player
    position = Actor.position player
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
   PRE:       True
   POST:
   EXAMPLES:  moveActors  ==
-}
moveActors :: GameState -> GameState
moveActors state@(State l s (Actor.Actors player ai) t)
  | levelGoal > s = State level score (Actor.Actors newPlayer newAI) t
  | otherwise     = Splash "Level done!" (nextState state)
  where
    levelGoal      = Level.getLevelGoal l
    newAI          = map (Actor.makeMove aiSpeed) ai
    newPlayer      = Actor.makeMove playerSpeed player
    (level, score) = case Level.checkForTreasure l (closestTile (Actor.position newPlayer)) of
                      Just l' -> (l', s + 1)
                      Nothing -> (l, s)
moveActors state = state -- On game over!

{- setMovement s
   PRE:       True
   POST:      s with new movements for its entities.
   EXAMPLES:  setMovement ==
-}
setMovement :: GameState -> GameState
setMovement (State level s (Actor.Actors player ai) t) =
  let
    board       = Level.getBoard level
    (newAI, t') = spawnAI level (t + timeStep) ai
  in
    if newAI `caughtPlayer` player
      then Splash "Game Over!" initialState
      else State level s (Actor.Actors (setPlayerMovement board player) (setAIMovements board player newAI)) t'
  where
    {- spawnAI (State b s p cs t)
       PRE:
       POST:     The state with a new ghost if t > spawnTime and length cs < maxAI
       EXAMPLES:
    -}
    spawnAI :: Level.Level -> Float -> [Actor.Actor] -> ([Actor.Actor], Float)
    spawnAI level t cs =
      let
        coords = Level.spawnPosition level
        nextAI = Actor.createAI coords (0,0) []
      in
        if t > spawnTime && length cs < maxAI
          then ((nextAI:cs), 0)
          else (cs, t)
    caughtPlayer :: [Actor.Actor] -> Actor.Actor -> Bool
    caughtPlayer []            _ = False
    caughtPlayer (ai:ais) player = (hasReachedDestination aiSpeed (Actor.position ai) (round x, round y))
      where
        (x, y) = Actor.position player
{- setAIMovements b p c
   PRE:           True
   POST:
   EXAMPLES:      setAIMovements  ==
-}
setAIMovements :: Board -> Actor.Actor -> [Actor.Actor] -> [Actor.Actor]
setAIMovements _ _ [] = []
setAIMovements board p c = map (setAIMovement board (Actor.position p)) c
  where
    {- setAIMovement b xy c
       PRE:           True
       POST:
       EXAMPLES:      setAIMovement  ==
       VARIANT:       |c|
    -}
    setAIMovement :: Board -> (Float, Float) -> Actor.Actor -> Actor.Actor
    setAIMovement board xy c
      | null paths     = Actor.createAI position direction (calculateAIMovement board xy position)
      | zero direction = changeAIDirection position [(closestTile position)] (calculateAIMovement board xy position)
      | otherwise =
        if (hasReachedDestination aiSpeed position (head paths))
          then changeAIDirection position paths (calculateAIMovement board xy position)
          else Actor.createAI position direction paths
      where
        position  = Actor.position c
        direction = Actor.direction c
        paths     = Actor.paths c
        {- calculateAIMovement b d s
           PRE:       True
           POST:      Shortest path in b from s to d
           EXAMPLES:  calculateAIMovement ==
        -}
        calculateAIMovement :: Board -> (Float, Float) -> (Float, Float) -> [(Int, Int)]
        calculateAIMovement board destination start =
          let
            d = closestTile destination
            s = closestTile start
          in
            case (aStar board d s) of
            [] -> [s]
            ps -> ps
        {- changeAIDirection oldPath newPath
           PRE:       oldPath and newPath must not be empty.
           POST:      ... a computer with
           EXAMPLES:  changeAIDirection ==
        -}
        -- TODO: WRITE BETTER FUNCTION SPECIFICATION
        changeAIDirection :: (Float, Float) -> [(Int, Int)] -> [(Int, Int)] -> Actor.Actor
        changeAIDirection position oldPath@((x, y):_) newPath@((x', y'):_) =
          let
            -- Calculates the next direction?
            direction = (fromIntegral x', fromIntegral y') - (fromIntegral x, fromIntegral y)
          in
            Actor.createAI position direction newPath

{- setPlayerMovement arguments
   PRE:       True?
   POST:      ...
   EXAMPLES:  setPlayerMovement ==
-}
setPlayerMovement :: Board -> Actor.Actor -> Actor.Actor
setPlayerMovement board player
  | zero direction && zero nextDirection = player
  | zero (direction + nextDirection)     = Actor.createPlayer position nextDirection nextDirection sprites -- fixes 180 movement delay bug.
  | otherwise =
    if (hasReachedDestination playerSpeed position (closestTile position))
      then Actor.createPlayer position (changePlayerDirection board position direction nextDirection) nextDirection sprites
      else player
  where
    (position, (direction, nextDirection), sprites) = (Actor.position player, Actor.directions player, Actor.sprites player)
    {- setPlayerDirection b c d n
       PRE:       True
       POST:      If tile at position c + d is valid, then d. Otherwise n
       EXAMPLES:  setPlayerDirection ==
    -}
    changePlayerDirection :: Board -> (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
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
   PRE:       True
   POST:      A nearest whole number position to p.
   EXAMPLES:  closestTile ==
-}
closestTile :: (Float, Float) -> (Int, Int)
closestTile (a, b) = (round a, round b)

{- isValidMove b p
   PRE:       b must have element with index p
   POST:      True if element with key p in b is a valid tile to move to, otherwise False.
   EXAMPLES:  isValidMove board (0,0) gives True if element with key (0,0) is a floor.
-}
isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board position =
  case (board ! position) of
    (Tile.Floor _ _) -> True
    _           -> False

{- approximatePosition p s
   PRE:       True
   POST:      The nearest whole number position to p if the distance between it and p is less than os, otherwise p.
   EXAMPLES:  approximatePosition  ==
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
   PRE:       True
   POST:      True if distance between p1 and p2 is less than s, otherwise False.
   EXAMPLES:  hasReachedDestination ==
-}
hasReachedDestination :: (Float, Float) -> (Float, Float) -> (Int, Int) -> Bool
hasReachedDestination (speed,_) current (x, y) = (approximatePosition current speed) == (fromIntegral x, fromIntegral y)
