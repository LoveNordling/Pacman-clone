import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import qualified Core.GraphicsEngine as GraphicsEngine
import qualified Core.GameEngine as GameEngine
--import qualified Core.Board.Tile as Tile
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Board as Board
import qualified Core.Board.GameState as GameState
import qualified Core.AI as AI

main :: IO ()
main = hspec $ do
    describe "Graphics Engine Test Suite" $ do
        fromHUnitTest GraphicsEngine.testSuite
    describe "AI Test Suite" $ do
        fromHUnitTest AI.testSuite
    describe "Tile Test Suite" $ do
        fromHUnitTest Tile.testSuite
