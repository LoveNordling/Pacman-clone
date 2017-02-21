module Core.GameEngine (step, handleKeyEvents, fps) where

import Data.Array
import Graphics.Gloss.Interface.Pure.Game

import Core.Board.GameState
import Core.Board.Actor
import Core.Board.Board
import Core.Board.Tile
import Core.AI

import Debug.Trace

-- The FPS of the game
fps :: Int
fps = 60

-- The player and AI speeds
playerSpeed, aiSpeed :: (Float, Float)
playerSpeed = actorSpeed 3.8 (fromIntegral fps)
aiSpeed     = actorSpeed 2 (fromIntegral fps)

{- actorSpeed s f
   PRE:       fps > 0
   POST:      A speed as the ratio of s and f
   EXAMPLES:  actorSpeed  ==
-}
actorSpeed :: Float -> Float -> (Float, Float)
actorSpeed speed fps = (speed / fps, speed / fps)

{- step s g
   PRE:       True
   POST:      g with updated state
   EXAMPLES:  step  ==
-}
step :: Float -> GameState -> GameState
step _ state = moveActors (setMovement state)

{- handleKeyEvents e g
   PRE:       True
   POST:      g with updated state if e is an event on arrow keys.
   EXAMPLES:  handleKeyEvents  ==
-}
handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey (SpecialKey k) Down _ _) s = changePlayerMovement s k
handleKeyEvents _ state = state

{- changePlayerMovement s
   PRE:       True
   POST:      s with updated desired direction for player.
   EXAMPLES:  changePlayerMovement ==
-}
changePlayerMovement :: GameState -> SpecialKey -> GameState
changePlayerMovement (State t (Player position direction nextDirection) c) k =
  let
    next = case k of
            KeyUp    -> (0, 1)
            KeyLeft  -> (-1,0)
            KeyDown  -> (0,-1)
            KeyRight -> (1, 0)
            _        -> nextDirection
  in
    State t (Player position direction next) c

{- moveActors s
   PRE:       True
   POST:
   EXAMPLES:  moveActors  ==
-}
moveActors :: GameState -> GameState
moveActors (State b (Player xy dx nd) (Computer pc dc path)) =
  let
    aiMovement = pc + dc * aiSpeed
    plMovement = xy + dx * playerSpeed
  in
    State b (Player plMovement dx nd) (Computer aiMovement dc path)

{- setMovement s
   PRE:       True
   POST:      s with new movements for its entities.
   EXAMPLES:  setMovement ==
-}
setMovement :: GameState -> GameState
setMovement (State b p@(Player _ _ d) c) = State b (setPlayerMovement b p) (setAIMovement b p c)

{- setAIMovement b p c
   PRE:       True
   POST:      ...
   EXAMPLES:  setAIMovement ==
-}
setAIMovement :: Board -> Actor -> Actor -> Actor
setAIMovement board (Player xy _ _) (Computer position direction path)
  | length path == 0   = Computer position direction (calculateAIMovement board xy position)
  | direction == (0,0) = changeAIDirection position [(nearestTile position)] (calculateAIMovement board xy position)
  | otherwise          =
    if (hasReachedDestination aiSpeed position (head path))
      then changeAIDirection position path (calculateAIMovement board xy position)
      else Computer position direction path
  where
    {- calculateAIMovement arguments
       PRE:           True
       POST:          post-condition on the result, in terms of the arguments
       SIDE EFFECTS:  None
       EXAMPLES:      calculateAIMovement  ==
       VARIANT:       None
    -}
    calculateAIMovement :: Board -> (Float, Float) -> (Float, Float) -> [(Int, Int)]
    calculateAIMovement board destination start =
      let
        d = nearestTile destination
        s = nearestTile start
      in
        case (aStar board d s) of
        [] -> [s]
        ps -> ps
    {- changeAIDirection oldPath newPath
       PRE:       True
       POST:      ... a computer with
       EXAMPLES:  changeAIDirection ==
    -}
    -- TODO: WRITE BETTER FUNCTION SPECIFICATION
    changeAIDirection :: (Float, Float) -> [(Int, Int)] -> [(Int, Int)] -> Actor
    changeAIDirection position oldPath@((x, y):_) newPath@((x', y'):_) =
      let
        -- Calculates the next direction?
        direction = (fromIntegral x', fromIntegral y') - (fromIntegral x, fromIntegral y)
      in
        Computer position direction newPath

{- setPlayerMovement arguments
   PRE:           pre-condition on the arguments
   POST:          post-condition on the result, in terms of the arguments
   EXAMPLES:      setPlayerMovement ==
-}
setPlayerMovement :: Board -> Actor -> Actor
setPlayerMovement board player@(Player position (0,0) (0,0)) = player
setPlayerMovement board player@(Player position direction nextDirection) =
    if (hasReachedDestination playerSpeed position (nearestTile position))
    then Player position (changePlayerDirection board position direction nextDirection) nextDirection
    else player
    where
      {- setPlayerDirection arguments
         PRE:           pre-condition on the arguments
         POST:          post-condition on the result, in terms of the arguments
         SIDE EFFECTS:  if any, including exceptions
         EXAMPLES:      setPlayerDirection ==
         VARIANT:       None
      -}
      changePlayerDirection :: Board -> (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float)
      changePlayerDirection board position direction nextDirection =
        let
          approxPosition = approximatePosition position (fst playerSpeed)
        in
          if isValidMove board (nearestTile (approxPosition + nextDirection))
            then nextDirection
            else if isValidMove board (nearestTile (approxPosition + direction))
              then direction
              else (0, 0)

{- nearestTile p
   PRE:       True
   POST:      A nearest whole number position to p.
   EXAMPLES:  nearestTile ==
-}
nearestTile :: (Float, Float) -> (Int, Int)
nearestTile (a, b) = (round a, round b)

{- isValidMove b p
   PRE:       True
   POST:      True if element with key p in b is a valid tile to move to, otherwise False.
   EXAMPLES:  isValidMove board (0,0) gives True if element with key (0,0) is a floor.
-}
isValidMove :: Board -> (Int, Int) -> Bool
isValidMove board position =
  case (board ! position) of
    (Floor _ _) -> True
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
    if abs (x - tx) < speed && abs (y - ty) < speed
      then (tx, ty)
      else (x, y)

{- hasReachedDestination s p1 p2
   PRE:       True
   POST:      True if distance between p1 and p2 is less than s, otherwise False.
   EXAMPLES:  hasReachedDestination ==
-}
hasReachedDestination :: (Float, Float) -> (Float, Float) -> (Int, Int) -> Bool
hasReachedDestination (speed,_) current (x, y) = (approximatePosition current speed) == (fromIntegral x, fromIntegral y)
