module Main where

-- External modules
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Internal modules
import qualified Core.GraphicsEngine as GraphicsEngine
import qualified Core.Board.GameState as GameState
import qualified Core.GameEngine as GameEngine

-- The window used by Gloss
window :: Display
window = InWindow "DazzleBox" (1000, 1000) (0, 0)

-- The background color of the window
backgroundColor :: Color
backgroundColor = black

{- main
   PRE:           True
   POST:          An instance of the The Game.
   SIDE EFFECTS:  Displays a window
   EXAMPLES:      main == a new window of size (1000, 1000) with the text "Press to Play" in red on a black background
-}
main :: IO ()
main = play window backgroundColor GameEngine.fps GameState.initialState GraphicsEngine.render GameEngine.handleKeyEvents GameEngine.step
