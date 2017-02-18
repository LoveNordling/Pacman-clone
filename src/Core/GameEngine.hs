module Core.GameEngine (step, handleKeyEvents) where

import Prelude hiding (Right, Left)
import Graphics.Gloss.Interface.Pure.Game
import Core.Board.Tile
import Core.Board.Board
import Core.Board.Actor
import Core.GraphicsEngine (tileSize)
import Tile


playerSize = 5
{- step s state
   PURPOSE:   Steps the game at s frames per second.
   PRE:       True
   POST:      state changed with new position for AI.
   EXAMPLES:  step ==
-}
step :: Float -> GameState -> GameState
step _ state@(State tiles p c) =
	let
		state = checkPlayerCollision p tiles playerSize (Core.GraphicsEngine.tileSize (length tiles)) playerSpeed state
	in moveActor state

-- TODO: Move this
fps :: Float
fps = 60

baseSpeed :: Float
baseSpeed = 5

actorSpeed :: Float -> Float -> (Float, Float)
actorSpeed speed fps = (speed / fps, speed / fps)

playerSpeed = fst (actorSpeed baseSpeed fps)

moveActor :: GameState -> GameState
moveActor (State t (Player p m) c) =
  let
    coordinates = p + m * (actorSpeed baseSpeed fps)
  in
    State t (Player coordinates m) c

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




checkPlayerCollision :: Actor -> Board -> Float -> Float -> Float -> GameState -> GameState
checkPlayerCollision _ [] _ _ _ state = state
checkPlayerCollision p (tile:tiles) playerSize tileSize speed state =
	if isColide tile
		then
			let state = checkPlayerCollisionAux tile playerSize tileSize speed state
			in checkPlayerCollision p tiles playerSize tileSize speed state

	else checkPlayerCollision p tiles playerSize tileSize speed state
	where
		isColide (Floor _) = False
		isColide w = isVerticalColide p tile playerSize tileSize speed && isHorizontalColide p tile playerSize tileSize speed
checkPlayerCollisionAux :: Tile -> Float -> Float -> Float -> GameState -> GameState
checkPlayerCollisionAux  (Wall (wx, wy)) playerSize tileSize speed state@(State t p@(Player (px, py) (hMove, vMove)) c) =
	let
		verticalColide :: GameState
		verticalColide =
			let
				py = wy -vMove*(playerSize/2 + tileSize/2)
				vMove = 0
			in (State t (Player (px, py) (hMove, vMove)) c)
		horrizontalColide :: GameState
		horrizontalColide =
			let
				px = wx -hMove*(playerSize/2 + tileSize/2)
				hMove = 0
			in (State t (Player (px, py) (hMove, vMove)) c)
	in if isVerticalColide p (Wall (wx, wy)) playerSize tileSize speed
		then
			let state = verticalColide
			in horrizontalColide
		else horrizontalColide



isVerticalColide :: Actor -> Tile -> Float -> Float -> Float -> Bool
isVerticalColide (Player (_, py) (_, vMove)) (Wall (_, wy)) playerSize tileSize speed = upColide && downColide
	where
		upColide = wy + tileSize/2 > py + vMove * speed - playerSize/2
		downColide = wy - tileSize/2 < py + vMove * speed + playerSize/2

isHorizontalColide :: Actor -> Tile -> Float -> Float -> Float -> Bool
isHorizontalColide (Player (px, _) (hMove, _)) (Wall (wx, _)) playerSize tileSize speed = rightColide && leftColide
	where
		rightColide = wx + tileSize/2 > px + hMove * speed - playerSize/2
		leftColide = wx - tileSize/2 < px + hMove * speed + playerSize/2








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
