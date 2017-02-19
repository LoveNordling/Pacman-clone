module Core.GraphicsEngine (render, tileSize) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Core.Board.Tile
import Core.Board.Board
import Core.Board.Actor
import Debug.Trace
import Tile
-- TODO: Should be in main.hs?
-- The size of the map
mapSize :: Int
mapSize = 1000

{- tileSize n
   PRE:       n > 0
   POST:      the calculated width and height of a tile based on n.
   EXAMPLES:  tileSize ==
-}
tileSize :: Int -> Float
tileSize n = 10 * sqrt (fromIntegral (mapSize) / fromIntegral (n))

{- drawMap t p c
   PRE:           True
   POST:          The image to be displayed, based on t with entities p and c.
   EXAMPLES:      render ==
-}
drawMap :: Board -> Actor -> Actor -> Picture
drawMap t p c =
  let dimensions = tileSize (length t)
      interior   = drawInterior t dimensions []
      user       = [drawActor p dimensions]
      ghost      = [drawActor c dimensions]
  in  Pictures (interior ++ user ++ ghost) --((drawInterior t d []) ++ [(drawActor p d)])
    where
      {- drawActor a f
         PRE:       True
         POST:      The actors a to be displayed.
         EXAMPLES:  drawActor ==
      -}
      drawActor :: Actor -> Float -> Picture
      drawActor (Player p _ _)   d = translateAndColor p d blue (circle 5)
      drawActor (Computer p) d = translateAndColor p d green (circle 5)
      {- drawInterior t f acc
         PRE:       True
         POST:      The interiors in t to be displayed.
         EXAMPLES:  drawInterior ==
         VARIANT:   |t|
      -}
      drawInterior :: Board -> Float -> [Picture] -> [Picture]
      drawInterior []             d acc = acc
      drawInterior ((Floor p):ts) d acc = drawInterior ts d ((makeRectangle p d red):acc)
      drawInterior ((Wall p):ts)  d acc = drawInterior ts d ((makeRectangle p d black):acc)

{- makeRectangle p d c
   PRE:       p must be valid coordinates.
   POST:      A rectangle of size d, color c on position p
   EXAMPLES:  makeRectangle ==
-}
makeRectangle :: (Float, Float) -> Float -> Color -> Picture
makeRectangle p d c = translateAndColor p d c (rectangleSolid d d)
{- translateAndColor p d c s
   PRE:       p must be valid coordinates
   POST:      Shape s positioned correctly based on p and d with color c.
   EXAMPLES:  translateAndColor ==
-}
translateAndColor :: (Float, Float) -> Float -> Color -> Picture -> Picture
translateAndColor p d c = (setCoordinates p d) . color c

{- setCoordinates (x, y) d p
   PRE:       True
   POST:      p with new coordinates based on x, y and d
   EXAMPLES:  setCoordinates ==
-}
setCoordinates :: (Float, Float) -> Float -> Picture -> Picture
setCoordinates (x, y) d p =
  let newX = x * d - (d/2)
      newY = y * d - (d/2)
  in  translate newX newY p

{- render g
   PRE:       True
   POST:      A map drawn from state of g.
   EXAMPLES:  render ==
-}
render :: GameState -> Picture
render (State t p c) = drawMap t p c
