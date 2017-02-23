import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Core.GraphicsEngine as GraphicsEngine
import qualified Core.GameEngine as GameEngine
import qualified Core.Board.Tile as Tile
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Board as Board
import qualified Core.Board.GameState as GameState
import Test.HUnit
import qualified Core.AI as AI
-- initialGameState = State Tile.standardTiles (Actor.Player (-5, -5)) (Actor.Computer (0, 0))
-- playerMovedState = State Tile.standardTiles (Actor.Player (-4, -5)) (Actor.Computer (0, 0))
-- playerMoveRightKey = EventKey (SpecialKey KeyRight) Down (Modifiers {shift = Up, ctrl = Up, alt = Up}) (0.0,0.0)

itemFloor = Tile.Floor (0, 0) True
baseFloor = Tile.Floor (0, 0) False
baseWall  = Tile.Wall (0, 0)

main :: IO ()
main = AI.testList
  
{-main :: IO ()
main = hspec $ do
  -- GRAPHICS ENGINE TESTS
  describe "Test tileSize" $ do
    -- Tile size
    it "Returns the calculated size of a tile when number of tiles == 5" $ do
      (GraphicsEngine.tileSize 5) `shouldBe` (round ( 10 * sqrt(1000/5)))
    it "Returns the calculated size of a tile when a number of tiles == (-2)" $ do
      -- sqrt of (x/-2) gives a complex number, which Int cannot represent
      (GraphicsEngine.tileSize (-2)) `shouldBe` (minBound)

  -- Render
  describe "Test render" $ do
    -- a map with 16 walls/floors and 2 entities
    it "returns a rendered map #1" $ do
      let board = [ [baseWall, baseWall, baseWall, baseWall], [baseWall, baseFloor, baseFloor, baseWall], [baseWall, baseFloor, baseFloor, baseWall], [baseWall, baseWall, baseWall, baseWall]]
      let (Pictures pics) = (GraphicsEngine.render (GameState.State (Board.createBoard board) (Actor.Player (0,0) (0,0) 0) (Actor.Computer (0,0) (0,0) [])))
      (length pics) `shouldBe` (18)

    -- map with just 2 entities drawn
    it "returns a rendered map #2" $ do
      let (Pictures pics) = (GraphicsEngine.render (GameState.State (Board.createBoard []) (Actor.Player (0,0) (0,0) 0) (Actor.Computer (0,0) (0,0) [])))
      (length pics) `shouldBe` (2)

  -- BOARD TESTS
  describe "Test createBoard" $ do
    it "generates a board" $ do
      let board     = [ [baseWall, baseWall, baseWall, baseWall], [baseWall, baseFloor, baseFloor, baseWall], [baseWall, baseFloor, baseFloor, baseWall], [baseWall, baseWall, baseWall, baseWall] ]
      (Board.createBoard board) `shouldBe` (array ((0,0),(3,3)) [((0,0),Tile.Wall (0,0)),((0,1),Tile.Wall (0,1)),((0,2),Tile.Wall (0,2)),((0,3),Tile.Wall (0,3)),((1,0),Tile.Wall (1,0)),((1,1),Tile.Floor (1,1) False),((1,2),Tile.Floor (1,2) False),((1,3),Tile.Wall (1,3)),((2,0),Tile.Wall (2,0)),((2,1),Tile.Floor (2,1) False),((2,2),Tile.Floor (2,2) False),((2,3),Tile.Wall (2,3)),((3,0),Tile.Wall (3,0)),((3,1),Tile.Wall (3,1)),((3,2),Tile.Wall (3,2)),((3,3),Tile.Wall (3,3))])
-}

--picture = (Pictures [Translate 97.5 97.5 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate 97.5 18.5 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate 97.5 (-60.5) (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate 97.5 (-139.5) (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate 18.5 97.5 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate 18.5 18.5 (Color (RGBA 1.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate 18.5 (-60.5) (Color (RGBA 1.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate 18.5 (-139.5) (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate (-60.5) 97.5 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate (-60.5) 18.5 (Color (RGBA 1.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate (-60.5) (-60.5) (Color (RGBA 1.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate (-60.5) (-139.5) (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate (-139.5) 97.5 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate (-139.5) 18.5 (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate (-139.5) (-60.5) (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate (-139.5) (-139.5) (Color (RGBA 0.0 0.0 0.0 1.0) (Polygon [(-39.5,-39.5),(-39.5,39.5),(39.5,39.5),(39.5,-39.5)])),Translate (-60.5) (-60.5) (Color (RGBA 0.0 0.0 1.0 1.0) (Circle 39.5)),Translate (-139.5) (-139.5) (Color (RGBA 0.0 1.0 0.0 1.0) (Circle 39.5))])
