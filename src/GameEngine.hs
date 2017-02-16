module GameEngine (update, handleKeyEvents) where
import Graphics.Gloss.Interface.Pure.Game hiding (Down, Up)
import Tile
import Prelude hiding (Right, Left)

{- update f s
   PRE:           True
   POST:          s without change
   EXAMPLES:      update ==
-}
update :: Float -> GameState -> GameState
update _ s = s

{- handleKeyEvents e g
   PRE:           True
   POST:          Game state g updated depending on e
   EXAMPLES:      handleKeyEvents  ==
-}
handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey (Char 'w') _ _ _) s = movePlayer s Up
handleKeyEvents (EventKey (Char 'a') _ _ _) s = movePlayer s Left
handleKeyEvents (EventKey (Char 's') _ _ _) s = movePlayer s Down
handleKeyEvents (EventKey (Char 'd') _ _ _) s = movePlayer s Right
handleKeyEvents _ s = s

{- moveActor g d
   PRE:           True
   POST:          Game state g with an entity moved in direction d
   EXAMPLES:      moveActor ==
-}
movePlayer :: GameState -> Direction -> GameState
movePlayer (State t (Player p) c) d =
  let
    m = case d of
          Up    -> p + (0, 1)
          Left  -> p + (-1, 0)
          Down  -> p + (0, -1)
          Right -> p + (1, 0)
  in
    if isValidMove t m
      then State t (Player m) c
      else State t (Player p) c

{- isValidMove t (x, y)
   PRE:           True
   POST:          True if floor with coordinates (x, y) in t is valid to move to, otherwise False.
   EXAMPLES:      isValidMove ==
   VARIANT:       |t|
-}
isValidMove :: [Tile] -> (Float, Float) -> Bool
isValidMove [] _ = True
isValidMove ((Floor p1 _):ts) p2
  | p1 == p2 = True
  | otherwise = isValidMove ts p2
isValidMove ((Wall p1):ts) p2
  | p1 == p2  = False
  | otherwise = isValidMove ts p2
