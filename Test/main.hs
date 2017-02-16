import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Graphics.Gloss.Interface.Pure.Game
import qualified Core.GraphicsEngine as GraphicsEngine
import qualified Core.GameEngine as GameEngine
import qualified Core.Board.Tile as Tile
import qualified Core.Board.Actor as Actor
import Tile

initialGameState = State Tile.standardTiles (Actor.Player (-5, -5)) (Actor.Computer (0, 0))
playerMovedState = State Tile.standardTiles (Actor.Player (-4, -5)) (Actor.Computer (0, 0))
playerMoveRightKey = EventKey (SpecialKey KeyRight) Down (Modifiers {shift = Up, ctrl = Up, alt = Up}) (0.0,0.0)
main :: IO ()
main = hspec $ do
  describe "Test tileSize" $ do
    it "returns the calculated size of a tile" $ do
      (GraphicsEngine.tileSize 5) `shouldBe` ((10*sqrt(1000/5)) :: Float)

  -- describe "Test handleKeyEvents" $ do
  --   it "returns a game state with player moved to the right" $ do
  --     (GameEngine.handleKeyEvents playerMoveRightKey) initialGameState `shouldBe` (playerMovedState :: Tile.GameState)
