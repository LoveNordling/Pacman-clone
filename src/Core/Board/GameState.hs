module Core.Board.GameState (GameState(..)) where
import Core.Board.Actor
import Core.Board.Tile
import Core.Board.Board

data GameState = State Board Actor Actor
