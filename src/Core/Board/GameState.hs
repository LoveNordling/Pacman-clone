module Core.Board.GameState
        ( GameState(..), initialState
        , newState, nextState
        )
where

import qualified Core.Board.Actor as Actor
import qualified Core.Board.Level as Level
import qualified Core.Extras.Resources as Resources


{-
  REPRESENTATION CONVENTION:
    The game state is given by State b a1 a2, where b is the map to be rendered, a1 is an actor and a2 is a list of actors.

    A splash screen is given by Slash str s, where str is a text to display, and s is a State representing the next state of the game.

  REPRESENTATION INVARIANT:
    Board and Actors in State must be non-empty.
-}

data GameState = State Level.Level Score Actor.Actors Float
               | Splash String GameState

-- Represents the score of the player
type Score = Int

-- The initial state
initialState :: GameState
initialState = Splash "Press to Play" (newState (Level.setLevel 0) Resources.playerSprites 0)

{- nextState l a
   PRE:       True
   POST:      Next state based on l?
   EXAMPLES:  nextState ==
-}
nextState :: GameState -> GameState
nextState (State level score (Actor.Actors player _) _) =
    newState (Level.nextLevel level) (Actor.sprites player) 0

{- newState l sp sc
   PRE:       True
   POST:      If l is Nothing, the state of game over with the next state, otherwise a state based on l, sp and sc.
   EXAMPLES:  newState ==
-}
newState :: Maybe (Level.Level, Actor.Position) -> [Actor.Sprite] -> Score -> GameState
newState Nothing sprites score = Splash ("You win!") initialState
newState (Just (level, position)) sprites score =
  let
    player = Actor.createPlayer position (0,0) (0,0) sprites
    actors = (Actor.Actors player [])
  in
    State level score actors 0
