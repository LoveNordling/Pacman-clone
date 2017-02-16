module Core.GameEngine (step, handleKeyEvents) where

import Prelude hiding (Right, Left)
import Graphics.Gloss.Interface.Pure.Game
import Core.Board.Tile
import Core.Board.Board
import Core.Board.Actor
import Tile

{- step s state
   PURPOSE:   Steps the game at s frames per second.
   PRE:       True
   POST:      state changed with new position for AI.
   EXAMPLES:  step ==
-}
step :: Float -> GameState -> GameState
step _ s = moveAI s

{- handleKeyEvents e g
   PURPOSE:   Moves the player on key events.
   PRE:       True
   POST:      Game state g updated depending on e
   EXAMPLES:  handleKeyEvents  ==
-}
handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey (SpecialKey k) Down _ _) s = movePlayer s k
handleKeyEvents _ s = s

{- moveActor g d
   PRE:           True
   POST:          Game state g with an entity moved in direction d
   EXAMPLES:      moveActor ==
-}
movePlayer :: GameState -> SpecialKey -> GameState
movePlayer (State t (Player p) c) k =
  let
    m = case k of
          KeyUp    -> p + (0, 1)
          KeyLeft  -> p + (-1, 0)
          KeyDown  -> p + (0, -1)
          KeyRight -> p + (1, 0)
  in
    if isValidMove t m
      then State t (Player m) c
      else State t (Player p) c

-- temporary function
moveAI :: GameState -> GameState
moveAI (State t p (Computer c)) =
  let
    m = c + (0, 1)
  in
  if isValidMove t m
    then State t p (Computer m)
    else State t p (Computer c)

{- isValidMove t (x, y)
   PRE:           True
   POST:          True if floor with coordinates (x, y) in t is valid to move to, otherwise False.
   EXAMPLES:      isValidMove ==
   VARIANT:       |t|
-}
isValidMove :: Board -> (Float, Float) -> Bool
isValidMove [] _ = True
isValidMove ((Floor p1 _):ts) p2
  | p1 == p2 = True
  | otherwise = isValidMove ts p2
isValidMove ((Wall p1):ts) p2
  | p1 == p2  = False
  | otherwise = isValidMove ts p2
