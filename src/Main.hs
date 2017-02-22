module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Core.GraphicsEngine as GraphicsEngine
import qualified Core.GameEngine as GameEngine
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Board as Board
import qualified Core.Board.Tile as Tile
import qualified Core.Board.Level as Level
import qualified Core.Board.GameState as GameState

import Data.Array

-- The window used by Gloss
window :: Display
window = InWindow "DazzleBox" (1000, 1000) (0, 0)

-- The background color of the window
backgroundColor :: Color
backgroundColor = white

-- Initial state of the game
state :: GameState.GameState
state = GameState.newState (Level.setLevel 0) 0

{- main
   PRE:       True
   POST:      An instance of the The Game.
   EXAMPLES:  main ==
-}
main :: IO ()
main = play window white GameEngine.fps state GraphicsEngine.render GameEngine.handleKeyEvents GameEngine.step
