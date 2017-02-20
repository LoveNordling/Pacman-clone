module Core.GameEngine (step, handleKeyEvents, fps) where
import Graphics.Gloss.Interface.Pure.Game
import Core.Board.Actor
import Core.Board.Board
import Core.Board.Tile
import Core.Board.GameState
import Core.AI
import Data.Array
import Debug.Trace
import Control.Concurrent

fps :: Int
fps = 60

computerSpeed :: (Float, Float)
computerSpeed = actorSpeed 1 (fromIntegral fps)

playerSpeed :: (Float, Float)
playerSpeed = actorSpeed 3 (fromIntegral fps)

actorSpeed :: Float -> Float -> (Float, Float)
actorSpeed speed fps = (speed / fps, speed / fps)

-- moves the player and the AI
step :: Float -> GameState -> GameState
step _ s = moveActor (checkPlayerCollsion (moveAI s)) --(movePlayer s)

handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey (SpecialKey k) Down _ _) s =
  setPlayerMovement s k
handleKeyEvents _ s = s



-- sets the movement of the player
setPlayerMovement :: GameState -> SpecialKey -> GameState
setPlayerMovement s@(State t (Player p moving w) c) k =
  let
    desiredDirection = case k of
                KeyUp    -> (0, 1)
                KeyLeft  -> (-1,0)
                KeyDown  -> (0,-1)
                KeyRight -> (1, 0)
                _        -> w
  in State t (Player p moving desiredDirection) c

checkPlayerCollsion :: GameState -> GameState
checkPlayerCollsion gst@(State board (Player (x,y) v w) c) | v + w == (0,0) = 
	State board (Player (x,y) w w) c
checkPlayerCollsion gst@(State board (Player (x,y) v w) c) = 
	let 
		(x',y') = approximatePosition (x,y) (fst playerSpeed)
		currentTile = (fromIntegral (round x), fromIntegral (round y))
	in
		if (x',y') == currentTile
		then makeValidMove (State board (Player (x' ,y') v w) c)
		else gst

makeValidMove :: GameState -> GameState
makeValidMove gst@(State board (Player (x,y) v w) c) =
	let
		tile2go = findTile board ((x,y) + w) 
		tile2come = findTile board ((x,y) + v) 
	in 
		if isValidMove tile2go
		then State board (Player (x,y) w w) c
		else
			if isValidMove tile2come 
			then State board (Player (x,y) v w) c
			else State board (Player (x,y) (0,0) w) c

isValidMove (Floor _ ) = True
isValidMove _ = False
			

findTile board (x,y) = board !(round x, round y)
	
moveActor :: GameState -> GameState
moveActor (State t (Player p m n) (Computer (x, y) (mx, my) ts)) =
  let
    aiMovement = (x, y) + ((mx, my) * computerSpeed)
    plMovement =  p + m * playerSpeed
  in
	State t (Player plMovement m n) (Computer aiMovement (mx, my) ts)

-- TODO: AI Movement must be rewritten.
-- Problem is, the fps rate is too high. The AI will make all its moves at once basically.
-- Perhaps: The AI will move one tile, recalculate, move to another tile, making the
-- movements smooth. Still, it doesn't solve the problem of the high fps rate.
moveAI :: GameState -> GameState
moveAI gst@(State t p (Computer m n [])) = State t p (Computer m n (calculateAIMovement gst))
moveAI gst@(State t p (Computer (x, y) (0,0) l)) = changeAIDirection (State t p (Computer (x, y) (0,0) [(round x, round y)]))
moveAI gst@(State t player (Computer position direction (destination:ts))) =
  if (hasReachedDestination computerSpeed position destination)
    then changeAIDirection gst
    else gst

{- changeAIDirection gst
   PRE:       ...
   POST:      ...
   EXAMPLES:  changeAIDirection ==
-}
changeAIDirection :: GameState -> GameState
changeAIDirection gst@(State t player (Computer current _ ((x,y):_))) =
  let
    path@((x', y'):tiles) = calculateAIMovement gst
    tile = (fromIntegral x', fromIntegral y')
    direction = tile - (fromIntegral x, fromIntegral y)
  in
    State t player (Computer current direction path)

{- hasReachedDestination s p1 p2
   PRE:       True
   POST:      True if distance between p1 and p2 is less than s, otherwise False.
   EXAMPLES:  hasReachedDestination ==
-}
hasReachedDestination :: (Float, Float) -> (Float, Float) -> (Int, Int) -> Bool
hasReachedDestination (speed,_) current (x, y) = (approximatePosition current speed) == (fromIntegral x, fromIntegral y)

{- approximatePosition p s
   PRE:       True
   POST:      The nearest whole number position to p if the distance between it and p is less than s, otherwise p.
   EXAMPLES:  approximatePosition ==
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

calculateAIMovement :: GameState -> [(Int, Int)]
calculateAIMovement (State t (Player (x, y) _ _) (Computer (a, b) _ _)) =
  let
     (x', y') = (round x, round y) -- computer tile
     (a', b') = (round a, round b) -- player tile
  in
    case (aStar t (x', y') (a', b')) of
      [] -> [(a', b')]
      ps -> ps
