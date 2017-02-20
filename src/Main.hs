module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Core.GraphicsEngine as GraphicsEngine
import qualified Core.GameEngine as GameEngine
import Core.Board.Tile
import Core.Board.Board
import Core.Board.Actor
import Tile

-- Add tuples, this perhaps should be in a different module
-- but I don't know how to export an instance?
instance (Num a, Num b) => Num (a, b) where
  (a, b) + (c, d) = (a+c, b+d)
  (a, b) - (c, d) = (a-c, b-d)
  (a, b) * (c, d) = (a*c, b*d)
  abs (a, b)      = (abs a, abs b)
  signum (a, b)   = (signum a, signum b)
  fromInteger a   = (fromInteger a, fromInteger a)

-- The initial state of the game
state :: GameState
state = State standardTiles (Player (-3, 3) (0,1) (0,0)) (Computer (0, 0))

-- The window used by Gloss
window :: Display
window = InWindow "DazzleBox" (1000, 1000) (0, 0)

-- The background color of the window
backgroundColor :: Color
backgroundColor = white

{- main
   PRE:       True
   POST:      An instance of the The Game.
   EXAMPLES:  main ==
-}
main :: IO ()
main = play window white 60 state GraphicsEngine.render GameEngine.handleKeyEvents GameEngine.step
