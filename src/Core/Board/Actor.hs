module Core.Board.Actor (Actor(..)) where

{-
  REPRESENTATION CONVENTION:
    A player character is given by Player (x, y) (a, b) s  where (x, y) is the initial x and y coordinates of the player, (a, b) is initial direction of the player and s is the score.
    A computer AI is given by Computer (c, d) (e, f) p where (c, d) is initial the x and y coordinates of the AI, (e, f) is the initial direction of the AI and p is the nearest path to the player.

  REPRESENTATION INVARIANT:
    The coordinates of the player and computer must not be out of bounds compared to the maps they are occupying. The components of the direction of the player and computer must be between -1 and 1.
-}
data Actor = Player   (Float, Float) Direction Direction
           | Computer (Float, Float) Direction Paths

-- Represents the score of a player.
type Score = Int

-- Paths of the AI
type Paths = [(Int, Int)]

-- First component is horizontal movement, second is vertical movement.
type Direction = (Float, Float)

{- position a
   PRE:       True
   POST:      The current position of a.
   EXAMPLES:  position (Player (5, -4)) == (5, -4)
-}
position :: Actor -> (Float, Float)
position (Player   (a, b) _ _) = (fromIntegral (round a), fromIntegral (round b))
position (Computer (a, b) _ _) = (fromIntegral (round a), fromIntegral (round b))
