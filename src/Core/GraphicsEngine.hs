module Core.GraphicsEngine (render, tileSize) where
import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Core.Board.Actor
import Core.Board.Board
import qualified Core.Board.Level as Level
import Core.Board.Tile
import Core.Board.GameState
import Core.Extras

{-# ANN module "HLint: Ignore Use mappend" #-}

mapSize :: Int
mapSize = 1000

{- tileSize n
   PRE:           n > 0
   POST:          the width and height of a tile based on n.
   EXAMPLES:      tileSize  ==
-}
tileSize :: Int -> Int
tileSize n = round (10 * sqrt ( fromIntegral (mapSize) / fromIntegral (n) ))

{- render s
   PRE:       True
   POST:      A map drawn from state of s.
   EXAMPLES:  render  ==
-}
render :: GameState -> Picture
render (State l _ (Actors p c) _) = drawMap (Level.getBoard l) p c

{- drawMap t p c
   PRE:           True
   POST:          The image to be displayed, based on t with entities p and c.
   EXAMPLES:      drawMap ==
-}
drawMap :: Board -> Actor -> [Actor] -> Picture
drawMap b p cs =
  let
    board      = elems b
    dimensions = tileSize (length board)
    interior   = drawInterior board dimensions []
    character  = [drawActor p dimensions]
    computer   = drawComputers cs
        where
            drawComputers [] = []
            drawComputers (c:cs) = [drawActor c dimensions] ++ (drawComputers cs)
  in
    Pictures (interior ++ character ++ computer)
      where
        {- drawActor a f
           PRE:       True
           POST:      The actors a to be displayed.
           EXAMPLES:  drawActor ==
        -}
        drawActor :: Actor -> Int -> Picture
        drawActor (Player   p _ _) d = translateAndColor p d blue (circle $ fromIntegral d / 2)
        drawActor (Computer p _ _) d = translateAndColor p d green (circle $ fromIntegral d / 2)
        {- drawInterior t f acc
           PRE:       True
           POST:      The interiors in t to be displayed.
           EXAMPLES:  drawInterior ==
           VARIANT:   |t|
        -}
        drawInterior :: Tiles -> Int -> [Picture] -> [Picture]
        drawInterior []             d acc = acc
        drawInterior ((Floor p True):ts) d acc = drawInterior ts d ((makeRectangle p d red):(makeCircle p d yellow):acc)
        drawInterior ((Floor p _):ts) d acc = drawInterior ts d ((makeRectangle p d red):acc)
        drawInterior ((Wall p):ts)  d acc = drawInterior ts d ((makeRectangle p d black):acc)
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
        translateAndColor :: (Position a) => (a, a) -> Int -> Color -> Picture -> Picture
        translateAndColor p d c = (setCoordinate p $ fromIntegral d) . color c
