module Core.Board.GameState
        ( GameState(..), initialState, newState, nextState, testSuite ) where

import Test.HUnit hiding (State)
import Data.Maybe
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Level as Level
import qualified Core.Extras.Sprite as Sprite
import qualified Core.Extras.Resources as Resources

{-
  REPRESENTATION CONVENTION:
    The game state is given by State l s a f, where b is a Level, s is the current player score, a is the actors and f is the next spawn time.

    A splash screen is given by Slash str s, where str is a text to display, and s is a State representing the next state of the game.

  REPRESENTATION INVARIANT:
    Score should not be greater than the level goal of the Level.
-}

data GameState = State Level.Level Score Actor.Actors Float
               | Splash String GameState
               deriving (Show, Eq)

-- Represents the score of the player
type Score = Int

-- The initial state
initialState :: GameState
initialState = Splash "Press to Play" (newState (Level.setLevel 0) 0)

{- nextState s
   PRE:           s should be a State
   POST:          A new state based on level number  s
   SIDE EFFECTS:  None
   EXAMPLES:      nextState (newState (Level.setLevel 0) Sprite.player 0) gives a new State with level 1
-}
nextState :: GameState -> GameState
nextState (State level score _ _) =
    newState (Level.nextLevel level) 0
nextState (Splash _ state) = state

{- newState (l, p) sc
   PRE:           True
   POST:          If l is Nothing, the state of game over with the initial state, otherwise a state with level l, the score sc and a player at position p.
   SIDE EFFECTS:  None
   EXAMPLES:      newState (Level.setLevel 0) Sprite.player 0 == the State with the first level, the score 0 and a player.
                  newState Nothing gives a splash page with the string "You win!" and the initial state as its next state.
-}
newState :: Maybe (Level.Level, Actor.Position) -> Score -> GameState
newState Nothing score = Splash ("You win!") initialState
newState (Just (level, position)) score = State level score actors 5
  where
    player = Actor.createPlayer position (0,0) (0,0) Sprite.player
    actors = (Actor.Actors player [])

-------------------------------------------
-- TEST CASES
-------------------------------------------
test1, test2, test3 :: Test
testSuite = TestList [ test1, test2, test3 ]
-- nextState
test1 =
  let
    splash@(Splash _ state) = initialState
  in
    TestLabel "Next State Test #1" .
      TestCase $ assertEqual "" (state) (nextState initialState)
-- newState
-- testing whether the position of the player is correct
test2 =
  let
    level = Level.setLevel 0
    (Just position) = case level of
                        Just (_,y) -> (Just y)
                        Nothing    -> (Just (0,0))
    (State _ _ (Actor.Actors p _) _) = newState level 0
  in
    TestLabel "Next State Test #1" .
      TestCase $ assertEqual "" (Actor.position p) (position)
test3 =
    TestLabel "Next State Test #2" .
      TestCase $ assertEqual "" (initialState) (nextState $ newState Nothing 0)
