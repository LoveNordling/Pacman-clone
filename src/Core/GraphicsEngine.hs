module Core.GraphicsEngine
        ( render, testSuite ) where

-- Modules for testing
import Test.HUnit

-- External modules
import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

-- Internal modules
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Board as Board
import qualified Core.Board.Level as Level
import qualified Core.Board.Tile as Tile
import qualified Core.Extras.Sprite as Sprite
import qualified Core.Extras.Common as Common
import qualified Core.Board.GameState as GameState
import qualified Core.Extras.Resources as Resources

{-# ANN module "HLint: Ignore Use mappend" #-}

-- The number of pixels of the game window
mapSize :: Int
mapSize = 1000

{- tileSize b
   PRE:           length of b > 0.
   POST:          The width and height of a tile based on b.
   SIDE EFFECTS:  None
   EXAMPLES:      tileSize (Board.createBoard (Resources.levels !! 0)) == 57
-}
tileSize :: Board.Board -> Int
tileSize b = 19 * round (sqrt ( fromIntegral (mapSize) / fromIntegral (length b) ))

{- render s
   PRE:           True
   POST:          s converted to a 2D representation to be displayed on screen
   SIDE EFFECTS:  None
   EXAMPLES:      render (GameState.initialState) == The text "Press to Play" to display on screen
-}
render :: GameState.GameState -> Picture
render (GameState.State l _ (Actor.Actors p c) _) = drawMap (Level.getBoard l) p c
render (GameState.Splash s _) = drawText s

{- drawText s
   PRE:           True
   POST:          s converted to text to display on screen
   SIDE EFFECTS:  None
   EXAMPLES:      drawText "Foo" == The text "Foo" colored red with x coordinate -100 from origin and y coordinate -30 from origin.
-}
drawText :: String -> Picture
drawText s = (translate (-110) (-30) . color red) (Scale 0.3 0.3 (Text s))
-- Hardcoded because it's not possible to calculate  width/height of Text

{- drawMap b p c
   PRE:           b must be non empty
   POST:          A 2D map of elements in t, with p and c in it
   SIDE EFFECTS:  None
   EXAMPLES:      drawMap (Board.createBoard (Resources.levels !! 0)) (Actor.createPlayer (1,1) (0,0) (0,0) Sprite.player) [] == a list of 104 pictures of a 2D map representing the first element of Resources.levels, with a player drawn on tile with position (1, 1) with sprites from Sprite.player
                  drawMap (Board.createBoard [[]]) (Actor.createPlayer (1,1) (0,0) (0,0) Sprite.player) [] == a list with only one element representing the player to be displayed on origo
-}
drawMap :: Board.Board -> Actor.Actor -> [Actor.Actor] -> Picture
drawMap b p cs =
  let
    board      = elems b
    dimensions = tileSize b
    interior   = drawInterior board dimensions []
    character  = [drawActor dimensions p]
    computer   = map (drawActor dimensions) cs
  in
    Pictures (interior ++ character ++ computer)
      where
        {- drawActor d a
           PRE:           True
           POST:          a converted to its 2D representation with its position based on d and position of a
           SIDE EFFECTS:  None
           EXAMPLES:      drawActor 10 (Actor.createPlayer (1,1) (0,0) (0,0) Sprite.player) == (Sprite.player !! 1) with x and y coordinates set to -10 from origo, scaled 0.1 times
        -}
        drawActor :: Int -> Actor.Actor -> Picture
        drawActor d a = translateAndColor p d green (Scale 0.1 0.1 s)
          where (p,s) = (Actor.position a, Actor.getPicture a)
        {- drawInterior t d acc
           PRE:           True
           POST:          acc with elements in t converted to their 2D representations with positions based on the elements position and d
           SIDE EFFECTS:  None
           EXAMPLES:      drawInterior [ Tile.Wall (0,0), Tile.Wall (0,0) ] 10 [] == Two pictures of rectangles, both set to be displayed on position (-20, -20) from origo
           VARIANT:       |t|
        -}
        drawInterior :: Board.Tiles -> Int -> [Picture] -> [Picture]
        drawInterior []                       _ acc = acc
        drawInterior ((Tile.Floor p True):ts) d acc = drawInterior ts d ((makeRectangle p d (greyN 0.8)):(makeCircle p d yellow):acc)
        drawInterior ((Tile.Floor p _):ts)    d acc = drawInterior ts d ((makeRectangle p d (greyN 0.8)):acc)
        drawInterior ((Tile.Wall p):ts)       d acc = drawInterior ts d ((makeRectangle p d black):acc)
        {- makeRectangle p d c
           PRE:           True
           POST:          A rectangle of size d, color c with position based on p and d
           SIDE EFFECTS:  None
           EXAMPLES:      makeRectangle (5,5) 10 red == A picture of a red rectangle with dimensions 10x10 and position (30, 30) from origo
        -}
        makeRectangle :: (Int, Int) -> Int -> Color -> Picture
        makeRectangle p d c = translateAndColor p d c (rectangleSolid (fromIntegral d) (fromIntegral d))
        {- makeCircle p d c
           PRE:           True
           POST:          A circle of size d, color c with position based on p and d
           SIDE EFFECTS:  None
           EXAMPLES:      makeCircle (1,1) 10 red == A picture of a red circle with radius d / 3 on position (-10, -10) from origo

        -}
        makeCircle :: (Int, Int) -> Int -> Color -> Picture
        makeCircle p d c = translateAndColor p d c (circleSolid (fromIntegral (d `div` 3)))
        {- translateAndColor p d c s
           PRE:           True
           POST:          Shape s with position based on p and d and with color c
           SIDE EFFECTS:  None
           EXAMPLES:      translateAndColor (1, 1) 20 black (rectangleSolid 10 10) == A rectangle with dimensions 10x10, colored black and position (-20,-20) from origo

        -}
        translateAndColor :: (Common.Position a) => (a, a) -> Int -> Color -> Picture -> Picture
        translateAndColor p d c = Common.setCoordinate p (fromIntegral d) rowLength . (color c)
          where rowLength = fromIntegral (round (sqrt (fromIntegral (length b))))

-------------------------------------------
-- TEST CASES
-------------------------------------------
test1, test2, test3, test4, test5 :: Test
testSuite = TestList [  test1, test2, test3, test4, test5 ]
-- Tile size
test1 = TestLabel "Tile Size Test #1" . TestCase $ assertEqual "Should return tile size" (minBound) (tileSize (Board.createBoard [[]]))
test2 = TestLabel "Tile Size Test #2" . TestCase $ assertEqual "Should return tile size" (result) (tileSize board)
  where board  = Board.createBoard (Resources.levels !! 0)
        result = 19 * round (sqrt (fromIntegral (mapSize) / fromIntegral (length board)))
-- Draw Map
test3 =
  let
    b = Board.createBoard [ [Tile.Wall (0,0), Tile.Wall (0,0), Tile.Wall (0,0), Tile.Wall (0,0)], [Tile.Wall (0,0), Tile.Floor (0,0) False, Tile.Floor (0,0) False, Tile.Wall (0,0)], [Tile.Wall (0,0), Tile.Floor (0,0) False, Tile.Floor (0,0) False, Tile.Wall (0,0)], [Tile.Wall (0,0), Tile.Wall (0,0), Tile.Wall (0,0), Tile.Wall (0,0)]]
    p = Actor.createPlayer (0,0) (0,0) (0,0) []
    c = [Actor.createAI (0,0) (0,0) [] []]
    (Pictures pics) = (drawMap b p c)
  in
    TestLabel "Draw Map Test #1" . TestCase $ assertEqual "Should return 18 elements" (18) (length pics)
test4 =
  let
    b = Board.createBoard [[]]
    p = Actor.createPlayer (0,0) (0,0) (0,0) []
    c = [Actor.createAI (0,0) (0,0) [] []]
    (Pictures pics) = (drawMap b p c)
  in
    TestLabel "Draw Map Test #2" . TestCase $ assertEqual "Should return 2 elements" (2) (length pics)
-- drawText
test5 = TestLabel "Draw Text Test #1" . TestCase $ assertEqual "Should return position" ((-110, -30)) ((x, y))
  where (Translate x y _) = drawText "This is a test"
