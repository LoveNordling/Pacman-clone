module Core.GraphicsEngine
        ( render, testSuite )
where

import Test.HUnit
import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

import qualified Core.Board.GameState as GameState
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Board as Board
import qualified Core.Board.Level as Level
import qualified Core.Board.Tile as Tile
import qualified Core.Extras.Common as Common
import qualified Core.Extras.Resources as Resources

{-# ANN module "HLint: Ignore Use mappend" #-}

mapSize :: Int
mapSize = 1000

{- tileSize n
   PRE:           n > 0
   POST:          the width and height of a tile based on n.
   EXAMPLES:      tileSize  ==
-}
tileSize :: Board.Board -> Int
--tileSize b = 40         --TODO Change definitions of tileSize to constant
tileSize b = 19 * round (sqrt ( fromIntegral (mapSize) / fromIntegral (length b) ))
{- mapDimensions b
   PRE:       True.
   POST:      Dimensions of b.
   EXAMPLES:  mapDimensions ==
-}
mapDimensions :: Board.Board -> (Int, Int)
mapDimensions b = fst $ bounds b

{- render s
   PRE:       True
   POST:      A map drawn from state of s.
   EXAMPLES:  render  ==
-}
render :: GameState.GameState -> Picture
render (GameState.State l _ (Actor.Actors p c) t) = drawMap (Level.getBoard l) p c t
render (GameState.Splash s _) = drawText s

{- drawText s
   PRE:       True
   POST:      ...
   EXAMPLES:  drawText  ==
-}
drawText :: String -> Picture
drawText s = (translate (-110) (-30) . color red) (Scale 0.3 0.3 (Text s))
-- Hardcoded because it's not possible tocalculate type Text width/height

{- drawMap t p c
   PRE:           True
   POST:          The image to be displayed, based on t with entities p and c.
   EXAMPLES:      drawMap ==
-}
drawMap :: Board.Board -> Actor.Actor -> [Actor.Actor] -> Float -> Picture
drawMap b p cs time =
  let
    board      = elems b
    dimensions = tileSize b
    interior   = drawInterior board dimensions []
    character  = [drawActor dimensions time p]
    computer   = map (drawActor dimensions time) cs
  in
    Pictures (interior ++ character ++ computer)
      where
        {- drawActor a f
           PRE:       True
           POST:      The actors a to be displayed.
           EXAMPLES:  drawActor ==
        -}
        drawActor :: Int -> Float -> Actor.Actor -> Picture
        drawActor d time a = translateAndColor p d green (Scale 0.1 0.1 s)
          where
            p = Actor.position a
            n = Actor.direction a
            s = Actor.getPicture a
            t = round time
        {- drawInterior t f acc
           PRE:       True
           POST:      The interiors in t to be displayed.
           EXAMPLES:  drawInterior ==
           VARIANT:   |t|
        -}
        drawInterior :: Board.Tiles -> Int -> [Picture] -> [Picture]
        drawInterior []                       _ acc = acc
        drawInterior ((Tile.Floor p True):ts) d acc = drawInterior ts d ((makeRectangle p d (greyN 0.8)):(makeCircle p d yellow):acc)
        drawInterior ((Tile.Floor p _):ts)    d acc = drawInterior ts d ((makeRectangle p d (greyN 0.8)):acc)
        drawInterior ((Tile.Wall p):ts)       d acc = drawInterior ts d ((makeRectangle p d black):acc)
        {- makeRectangle p d c
           PRE:       p must be valid coordinates.
           POST:      A rectangle of size d, color c on position p
           EXAMPLES:  makeRectangle ==
        -}
        makeRectangle :: (Int, Int) -> Int -> Color -> Picture
        makeRectangle p d c = translateAndColor p d c (rectangleSolid (fromIntegral d) (fromIntegral d))
        {- makeCircle p d c
           PRE:       p must be valid coordinates.
           POST:      A circle of size d, color c on position p.
           EXAMPLES:  makeCircle ==
        -}
        makeCircle :: (Int, Int) -> Int -> Color -> Picture
        makeCircle p d c = translateAndColor p d c (circleSolid (fromIntegral (d `div` 3)))
        {- translateAndColor p d c s
           PRE:       p must be valid coordinates
           POST:      Shape s positioned based on p and d with color c.
           EXAMPLES:  translateAndColor ==
        -}
        translateAndColor :: (Common.Position a) => (a, a) -> Int -> Color -> Picture -> Picture
        translateAndColor p d c = Common.setCoordinate p (fromIntegral d) rowLength . (color c)
          where rowLength = fromIntegral (round (sqrt (fromIntegral (length b))))


-------------------------------------------
-- TEST CASES
-------------------------------------------
test1, test2, test3, test4 :: Test
testSuite = TestList [  test1, test2, test3, test4 ]
-- Tile size
test1 = TestLabel "Tile size #1" $ TestCase $ assertEqual "" (minBound) (tileSize (Board.createBoard [[]]))
test2 = TestLabel "Tile size #2" $ TestCase $ assertEqual "" (result) (tileSize board)
    where
        level = Resources.levels !! 0
        board = Board.createBoard level
        result = 19 * round (sqrt (fromIntegral (mapSize) / fromIntegral (length board)))
-- Draw Map
-- a map with 16 walls/floors and 2 entities
test3 =
    let
        b = Board.createBoard [ [Tile.Wall (0,0), Tile.Wall (0,0), Tile.Wall (0,0), Tile.Wall (0,0)], [Tile.Wall (0,0), Tile.Floor (0,0) False, Tile.Floor (0,0) False, Tile.Wall (0,0)], [Tile.Wall (0,0), Tile.Floor (0,0) False, Tile.Floor (0,0) False, Tile.Wall (0,0)], [Tile.Wall (0,0), Tile.Wall (0,0), Tile.Wall (0,0), Tile.Wall (0,0)]]
        p = Actor.createPlayer (0,0) (0,0) (0,0) []
        c = [Actor.createAI (0,0) (0,0) [] []]
        (Pictures pics) = (drawMap b p c 0)
    in
        TestLabel "Draw map #1" $ TestCase $ assertEqual "" (18) (length pics)
-- a map with just 2 entities
test4 =
    let
        b = Board.createBoard [[]]
        p = Actor.createPlayer (0,0) (0,0) (0,0) []
        c = [Actor.createAI (0,0) (0,0) [] []]
        (Pictures pics) = (drawMap b p c 0)
    in
        TestLabel "Draw map #2" $ TestCase $ assertEqual "" (2) (length pics)
