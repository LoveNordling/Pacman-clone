module Core.Board.Actor (Actor(..), position) where

{-
  REPRESENTATION CONVENTION:
    A player is given by Player (a, b) v w where a is the x-coordinate and b is the y-coordinate of the player, v is where the player is moving and w is where the player wants to move. A computer AI is given by Computer (c, d) where c is the x-coordinate and d is the y-coordinate of the AI.
  REPRESENTATION INVARIANT:
    The coordinates of the player and computer must not be out of bounds compared to the board they are occupying.

-}
data Actor = Player (Float, Float) Movement Movement | Computer (Float, Float)

-- First component is horizontal movement, second is vertical
type Movement = (Float, Float)

{- position a
   PRE:       True
   POST:      The current position of a.
   EXAMPLES:  position (Player (5, -4)) == (5, -4)
-}
position :: Actor -> (Float, Float)
position (Player a _ _)   = a
position (Computer a) = a
