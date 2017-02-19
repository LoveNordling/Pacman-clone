module Core.GameEngine (step, handleKeyEvents) where
import Graphics.Gloss.Interface.Pure.Game
import Core.Board.Actor
import Core.Board.Board
import Core.Board.Tile
import Core.Board.GameState
import Core.AI
import Data.Array
import Debug.Trace

-- moves the player and the AI
step :: Float -> GameState -> GameState
step _ s = moveAI (movePlayer s)

handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey (SpecialKey k) Down _ _) s =
  setPlayerMovement s k
handleKeyEvents _ s = s

actorSpeed :: Float -> Float -> (Float, Float)
actorSpeed speed fps = (speed / fps, speed / fps)

-- sets the movement of the player
setPlayerMovement :: GameState -> SpecialKey -> GameState
setPlayerMovement s@(State t (Player p moving) c) k =
  let
    movement = case k of
                KeyUp    -> (0, 1)
                KeyLeft  -> (-1,0)
                KeyDown  -> (0,-1)
                KeyRight -> (1, 0)
                _        -> moving
  in
    State t (Player p movement) c

movePlayer :: GameState -> GameState
movePlayer s@(State t (Player p m) (Computer c _)) =
  let
    n = p + m * (actorSpeed 3 60)
    (_:is) = calculateAIMovement s
    -- b = (fromIntegral x, fromIntegral y)
  in
    if isValidMove t n
      then State t (Player n m) (Computer c is)
      else State t (Player p (0, 0)) (Computer c is)

moveAI :: GameState -> GameState
moveAI gst@(State _ _ (Computer _ [])) = gst
moveAI (State t p (Computer c@(r,s) ((x,y):cs))) =
  let
     m = (fromIntegral (x - (round r)), fromIntegral (y - (round s)))
     b = (fromIntegral x, fromIntegral y) + m * (actorSpeed 5 60)
  in
    State t p (Computer b cs)

isValidMove :: Board -> (Float, Float) -> Bool
isValidMove board (x, y) =
  let
    (x', y') = (round x, round y)
  in
    case (board ! (x', y')) of
      (Wall _) -> False
      _        -> True

calculateAIMovement :: GameState -> [(Int, Int)]
calculateAIMovement (State t (Player (x, y) _) (Computer (a, b) _)) =
  let
     (x', y') = (round x, round y)
     (a', b') = (round a, round b)
  in
    case (aStar t (x', y') (a', b')) of
      [] -> [(a', b')]
      ps -> ps
