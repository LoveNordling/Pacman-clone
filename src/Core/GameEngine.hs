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
step _ s = moveActor s

-- TODO: Move this
fps :: Float
fps = 60

baseSpeed :: Float
baseSpeed = 5

actorSpeed :: Float -> Float -> (Float, Float)
actorSpeed speed fps = (speed / fps, speed / fps)

playerSpeed = actorSpeed baseSpeed fps

{- moveActor g
   PRE:      True
   POST:     TODO
   EXAMPLES: moveActor ==
-}
moveActor :: GameState -> GameState
moveActor (State t (Player p m w) c) =
  let
    coordinates = p + m * playerSpeed
  in
    State t (Player coordinates m w) c

{- handleKeyEvents e g
   PURPOSE:   Moves the player on key events.
   PRE:       True
   POST:      Game state g updated depending on e
   EXAMPLES:  handleKeyEvents  ==
-}
handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey (SpecialKey k) Down _ _) s = movePlayer s k
--handleKeyEvents (EventKey (SpecialKey k) Up   _ _) s = stopPlayer s k
handleKeyEvents _ s = s


movePlayer :: GameState -> SpecialKey -> GameState
movePlayer (State t (Player p (x, y) w) c ) k =
  let
    m = case k of
          KeyUp    -> (0, 1)
          KeyLeft  -> (-1,0)
          KeyDown  -> (0,-1)
          KeyRight -> (1, 0)
          _        -> w
  in makeValidMove (State t (Player p (x, y) m) c)


makeValidMove :: GameState -> GameState
makeValidMove s@(State t (Player p (x, y) m) c) =
	let 
		p = approxPlayerPos (Player p (x, y) m)
		tile2Go = findTile (p + m) --The tile that arrives with respect to wished direction
		tile2Come = findTile (p + (x, y)) --The tile that arrives with respect to current direction
	in 
	  if isValidTile tile2Go
	    then State t (Player p m m) c
	    else 
		 if isValidTile  tile2Come 
		  then State t (Player p (x, y) m) c 
		  else State t(Player p (0, 0) m) c


  
isValidTile :: Maybe Tile -> Bool
isValidTile (Just (Floor p )) = True
isValidTile _ = False

findTile :: (Float, Float) -> Maybe Tile
findTile p = findTileAux p (Tile.standardTiles)

findTileAux :: (Float, Float) -> [Tile] -> Maybe Tile
findTileAux _ [] = Nothing
findTileAux p ((Wall m ):ts) | p == m = Just (Wall p)
findTileAux p ((Floor m ):ts) | p == m = Just (Floor p)
findTileAux p (_:ts) = findTileAux p ts

approxPlayerPos :: Actor -> (Float, Float)
approxPlayerPos p@(Player (x,y) (mx, my) m)
	| let tx = fst (findPlayerTile p) in (x + mx * (fst playerSpeed) - tx)^2 < (fst playerSpeed) ^ 2 = 
	    let tx = fst (findPlayerTile p) in (tx, y)
	| let ty = snd (findPlayerTile p) in (y + my * (fst playerSpeed) - ty)^2 < (fst playerSpeed) ^ 2 = 
	    let ty = snd (findPlayerTile p) in (x, ty)
	| otherwise = (x,y)


findPlayerTile :: Actor -> (Float, Float)
findPlayerTile (Player (x,y) _ _) = (fromIntegral (round x), fromIntegral (round y))


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	--OLD MOVEMENT! 
	--REMOVE?
	
	
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
