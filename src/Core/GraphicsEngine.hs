module Core.GraphicsEngine (render, tileSize) where
import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Core.Board.Actor
import Core.Board.Board
import Core.Board.Tile
import Core.Board.GameState

import Debug.Trace

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
render (State t p c) = drawMap t p c

{- drawMap t p c
   PRE:           True
   POST:          The image to be displayed, based on t with entities p and c.
   EXAMPLES:      render ==
-}
drawMap :: Board -> Actor -> Actor -> Picture
drawMap b p c =
  let
    board      = elems b
    dimensions = tileSize (length board)
    interior   = drawInterior board dimensions []
    character  = [drawActor p dimensions]
    computer   = [drawActor c dimensions]
  in
    Pictures (interior ++ character ++ computer)
      where
        {- drawActor a f
           PRE:       True
           POST:      The actors a to be displayed.
           EXAMPLES:  drawActor ==
        -}
        drawActor :: Actor -> Int -> Picture
        drawActor (Player   p _) d = translateAndColor p d blue (circle $ fromIntegral d / 2)
        drawActor (Computer p _ _) d = translateAndColor p d green (circle $ fromIntegral d / 2)
        {- drawInterior t f acc
           PRE:       True
           POST:      The interiors in t to be displayed.
           EXAMPLES:  drawInterior ==
           VARIANT:   |t|
        -}
        drawInterior :: Tiles -> Int -> [Picture] -> [Picture]
        drawInterior []             d acc = acc
        drawInterior ((Floor p):ts) d acc = drawInterior ts d ((makeRectangle p d red):acc)
        drawInterior ((Wall p):ts)  d acc = drawInterior ts d ((makeRectangle p d black):acc)
        {- makeRectangle p d c
           PRE:       p must be valid coordinates.
           POST:      A rectangle of size d, color c on position p
           EXAMPLES:  makeRectangle ==
        -}
        makeRectangle :: (Int, Int) -> Int -> Color -> Picture
        makeRectangle p d c = translateAndColor p d c (rectangleSolid (fromIntegral d) (fromIntegral d))

        {- translateAndColor p d c s
           PRE:       p must be valid coordinates
           POST:      Shape s positioned based on p and d with color c.
           EXAMPLES:  translateAndColor ==
        -}
        translateAndColor :: (Position a) => (a, a) -> Int -> Color -> Picture -> Picture
        translateAndColor p d c = (setCoordinate p $ fromIntegral d) . color c



-- translateCoordinates :: (Float, Float) -> (Int, Int)
-- translateCoordinates (x, y) = (round x, round y)


class Position a where
  {- setCoordinate c d p
     PRE:       True
     POST:      p with new coordiantes based on c and d.
     EXAMPLES:  setCoordinate  ==
  -}
  setCoordinate :: (a, a) -> Int -> Picture -> Picture

instance Position Int where
  setCoordinate (x, y) d p =
    let
      newX = fromIntegral (x * d) - (fromIntegral d/2) - 100
      newY = fromIntegral (y * d) - (fromIntegral d/2) - 100
    in
      translate newX newY p

instance Position Float where
  setCoordinate (x, y) d p =
    let
      newX =  (x * (fromIntegral d)) - ( fromIntegral d / 2) - 100
      newY =  (y * (fromIntegral d)) - ( fromIntegral d / 2) - 100
    in
      translate newX newY p
