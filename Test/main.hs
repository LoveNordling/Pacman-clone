import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)

import qualified Core.GraphicsEngine as GraphicsEngine (testSuite)
import qualified Core.GameEngine as GameEngine --(testSuite)
import qualified Core.Board.Tile as Tile (testSuite)
import qualified Core.Board.Actor as Actor (testSuite)
import qualified Core.Board.Board as Board (testSuite)
import qualified Core.Board.Level as Level (testSuite)
import qualified Core.Board.GameState as GameState (testSuite)
import qualified Core.AI as AI (testSuite)

main :: IO ()
main = hspec $ do
    describe "Graphics Engine Test Suite" $ do
        fromHUnitTest GraphicsEngine.testSuite
    describe "AI Test Suite" $ do
        fromHUnitTest AI.testSuite
    describe "Tile Test Suite" $ do
        fromHUnitTest Tile.testSuite
    describe "Level Test Suite" $ do
        fromHUnitTest Level.testSuite
    describe "Board Test Suite" $ do
        fromHUnitTest Board.testSuite
    describe "Actor Test Suite" $ do
        fromHUnitTest Actor.testSuite
    describe "GameState Test Suite" $ do
        fromHUnitTest GameState.testSuite
    describe "" $ do
        GameEngine.testSuiteExceptions
