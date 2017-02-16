module GraphicsEngine (render) where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
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
drawMap :: [Tile] -> Actor -> Actor -> Picture
drawMap t p c =
  let
    d = tileSize (length t)
  in
    Pictures ((drawInterior t d []) ++ [(drawActor p d)])
  where
    {- drawActor a f
       PRE:       True
       POST:      The actors a to be displayed.
       EXAMPLES:  drawActor ==
    -}
    drawActor :: Actor -> Float -> Picture
    drawActor (Player p) d   = (setCoordinates p d) . color blue $ circle 5
    drawActor (Computer p) d = (setCoordinates p d) . color blue $ circle 5
    {- drawInterior t f acc
       PRE:       True
       POST:      The interiors in t to be displayed.
       EXAMPLES:  drawInterior ==
    -}
    drawInterior :: [Tile] -> Float -> [Picture] -> [Picture]
    drawInterior []               d acc = acc
    drawInterior ((Floor p _):ts) d acc = let m = (setCoordinates p d) . color red $ rectangleSolid d d
                                          in  drawInterior ts d (m:acc)
    drawInterior ((Wall p):ts)    d acc = let m = (setCoordinates p d) . color black $ rectangleSolid d d
                                          in  drawInterior ts d (m:acc)

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
