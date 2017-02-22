module Core.Board.GameState (GameState(..)) where
import Core.Board.Actor
import Core.Board.Tile
import Core.Board.Board

{-
  REPRESENTATION CONVENTION:
    The game state is given by State b a1 a2, where b is the map to be rendered, a1 is an actor and a2 is a list of actors.

  REPRESENTATION INVARIANT:
    Board must not be empty.
-}
data GameState = State Board Score Actor [Actor] Float

-- Represents the score of the player
type Score = Int
