module Core.GameEngine (step, handleKeyEvents) where

import Prelude hiding (Right, Left)
import Graphics.Gloss.Interface.Pure.Game
import Core.Board.Tile
import Core.Board.Board
import Core.Board.Actor
import Core.Pathfinder
import Tile
import Debug.Trace

{- step s state
   PURPOSE:   Steps the game at s frames per second.
   PRE:       True
   POST:      state changed with new position for AI.
   EXAMPLES:  step ==
-}
step :: Float -> GameState -> GameState
step _ s = moveAI (moveActor s)

-- TODO: Move this
fps :: Float
fps = 60

baseSpeed :: Float
baseSpeed = 5

actorSpeed :: Float -> Float -> (Float, Float)
actorSpeed speed fps = (speed / fps, speed / fps)


{- calculateAIMovement s
   PRE:           True
   POST:          a new path based on s
   EXAMPLES:      calculateAIMovement ==
-}
calculateAIMovement :: GameState -> [(Float, Float)]
calculateAIMovement (State t (Player p _) (Computer c _)) = case (aStar t p c) of
                                                              [] -> [c]
                                                              ps -> ps

moveActor :: GameState -> GameState
moveActor s@(State t (Player p m) _) =
  let
    coordinates = p + m -- * (actorSpeed baseSpeed fps)
    (c:cs) = calculateAIMovement s
  in
    State t (Player coordinates (0,0)) (Computer c cs)
    -- State t (Player coordinates m) (Computer c cs)

{- handleKeyEvents e g
   PURPOSE:   Moves the player on key events.
   PRE:       True
   POST:      Game state g updated depending on e
   EXAMPLES:  handleKeyEvents  ==
-}
handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey (SpecialKey k) Down _ _) s = movePlayer s k
handleKeyEvents (EventKey (SpecialKey k) Up   _ _) s = stopPlayer s k
handleKeyEvents _ s = s

{- moveActor g d
   PRE:      True
   POST:     Game state g with an entity moved in direction d
   EXAMPLES: moveActor ==
-}
movePlayer :: GameState -> SpecialKey -> GameState
movePlayer (State t (Player p (x, y)) c) k =
  let
    m = case k of
          KeyUp    -> (0, 1)
          KeyLeft  -> (-1,0)
          KeyDown  -> (0,-1)
          KeyRight -> (1, 0)
          _        -> (x, y)
  in
    State t (Player p m) c
    -- if isValidMove t n
    --   then State t (Player p m) c
    --   else State t (Player p (x, y)) c

stopPlayer :: GameState -> SpecialKey -> GameState
stopPlayer (State t (Player p (x, y)) c) k =
  let
    m = case k of
      KeyUp    -> (x, 0)
      KeyLeft  -> (0, y)
      KeyDown  -> (x,0)
      KeyRight -> (0, y)
      _        -> (x, y)
  in
    State t (Player p m) c


moveAI :: GameState -> GameState
moveAI gst@(State t p (Computer c [])) = gst
moveAI (State t p (Computer _ (c:cs))) = State t p (Computer c cs)

-- movePlayer :: GameState -> SpecialKey -> GameState
-- movePlayer (State t (Player p x) c) k =
--   let
--     m = case k of
--           KeyUp    -> (0, 0.2)--p + (0, 0.2)
--           KeyLeft  -> (-0.2, 0)--p + (-0.2, 0)
--           KeyDown  -> (0, -0.2)--p + (0, -0.2)
--           KeyRight -> (0.2, 0)--p + (0.2, 0)
--   in
--     if isValidMove t m
--       then State t (Player (p + m) (x + m)) c
--       else State t (Player p x) c

-- temporary function
-- moveAI :: GameState -> GameState
-- moveAI (State t p (Computer c)) =
--   let
--     m = c + (0, 1)
--   in
--   if isValidMove t m
--     then State t p (Computer m)
--     else State t p (Computer c)

{- isValidMove t (x, y)
   PRE:           True
   POST:          True if floor with coordinates (x, y) in t is valid to move to, otherwise False.
   EXAMPLES:      isValidMove ==
   VARIANT:       |t|
-}
-- isValidMove :: Board -> (Float, Float) -> Bool
-- isValidMove [] _ = True
-- isValidMove ((Floor p1 _):ts) p2
--   | p1 == p2 = True
--   | otherwise = isValidMove ts p2
-- isValidMove ((Wall p1):ts) p2
--   | p1 == p2  = False
--   | otherwise = isValidMove ts p2
