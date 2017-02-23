module Core.Board.GameState
        ( GameState(..), newState
        )
where

import qualified Core.Board.Actor as Actor
import qualified Core.Board.Level as Level


{-
  REPRESENTATION CONVENTION:
    The game state is given by State b a1 a2, where b is the map to be rendered, a1 is an actor and a2 is a list of actors.

  REPRESENTATION INVARIANT:
    Board must not be empty.
-}

data GameState = State Level.Level Score Actor.Actors Float
               | Splash String

-- Represents the score of the player
type Score = Int

newState :: Maybe (Level.Level, Actor.Position) -> [Actor.Sprite] -> Score -> GameState
newState Nothing _ score = Splash ("Game over. High score: " ++ show (score))
newState (Just (level, position)) sprites score =
  let
    player = Actor.createPlayer position (0,0) (0,0) sprites
    actors = (Actor.Actors player [])
  in
    State level score actors 0
