-----------------------------------------------------
--  DazzleBox
--  Graphic
--
--  Authors:
--    Ardalan Samimi, Love Nordling
--
--  Date:
--    February 15th, 2017
-----------------------------------------------------
module Graphic(renderMap) where

import Graphics.Gloss
import Tile
import Prelude hiding(Right, Left)

{-
    REPRESENTATION CONVENTION:
      Represents the direction a border should be drawn.
    REPRESENTATION INVARIANT:
      None.
-}
data BorderDirection = Top | Left | Bottom | Right

-- The width of a wall
wallWidth :: Float
wallWidth = 10

{- window x y
   PRE:           x and y must be bigger than 0
   POST:          a window with width x and height y pixels.
   EXAMPLES:      window 1000 1000 gives a window with height and width 1000 pixels.
-}
window :: Int -> Int -> Display
window x y = InWindow "Dazzlebox" (x, y) (0, 0)

{- background
   PRE:           True
   POST:          a color
   EXAMPLES:      background ==
-}
background :: Color
background = white

{- boardDimensions
   PRE:           True
   POST:          Width and height of the board.
   EXAMPLES:      boardDimensions
-}
boardDimensions :: Int
boardDimensions = 500

{- tileDimensions n
   PRE:           n > 0
   POST:          the width/height of an individual tile.
   EXAMPLES:      tileDimensions ==
-}
tileDimensions :: Int -> Float
tileDimensions n = 10 * sqrt ( fromIntegral (boardDimensions) / fromIntegral (n) )

{- drawMap b d
   PRE:       b is a valid board
   POST:      the game map based on b.
   EXAMPLES:  createBoard ==
-}
drawMap :: Board -> Picture
drawMap t d = Pictures (drawMapAux t [] [])
    where
      {- drawMapAux tiles t w
         PRE:           True?
         POST:          display elements t and w based on tiles
         EXAMPLES:      drawMapAux ==
         VARIANT:       |t|
      -}
      drawMapAux :: [(Position, Tile)] -> [Picture] -> [Picture] -> [Picture]
      drawMapAux []           tileAcc wallAcc = tileAcc ++ wallAcc
      drawMapAux (tile:tiles) tileAcc wallAcc = drawMapAux tiles ((drawTile tile d) ++ tileAcc) ((drawWall tile d) ++ wallAcc)
      {- drawWall arguments
           PRE:
           POST:
           EXAMPLES:      drawWall ==
        -}
      drawWall :: (Position, Tile) -> Float -> [Picture]
      drawWall ((x, y), (Tile (True, l, b, r) a)) d = (buildWall (x, y) d Top):(drawWall ((x, y), Tile (False, l, b, r) a) d)
      drawWall ((x, y), (Tile (_, True, b, r) a)) d = (buildWall (x, y) d Left):(drawWall ((x, y), Tile (False, False, b, r) a) d)

      drawWall ((x, y), (Tile (_, _, True, r) a)) d = (buildWall (x, y) d Bottom):(drawWall ((x, y), Tile (False, False, False, r) a) d)
      drawWall ((x, y), (Tile (_, _, _, True) a)) d = (buildWall (x, y) d Right):(drawWall ((x, y), Tile (False, False, False, False) a) d)
      drawWall ((x, y), (Tile (_, _, _, _) a)) d = []
      {- buildWall
         PRE:
         POST:
         EXAMPLES:      buildWall ==
      -}
      buildWall :: Position -> Float -> BorderDirection -> Picture
      buildWall (x, y) d Top     =
        Polygon [
          ( (x * d - (d/2) - wallWidth), (y * d + (d/2) + wallWidth) ),
          ( (x * d + (d/2) + wallWidth), (y * d + (d/2) + wallWidth) ),
          ( (x * d + (d/2) + wallWidth), (y * d + (d/2) - wallWidth) ),
          ( (x * d - (d/2) - wallWidth), (y * d + (d/2) - wallWidth) )
        ]
      buildWall (x, y) d Left    =
        Polygon [
          ( (x * d - (d/2)  + wallWidth), (y * d + (d/2)) + wallWidth),
          ( (x * d - (d/2)  + wallWidth), (y * d - (d/2)) - wallWidth),
          ( (x * d - (d/2) - wallWidth), (y * d - (d/2)) - wallWidth),
          ( (x * d - (d/2) - wallWidth), (y * d + (d/2) ) + wallWidth)
        ]
      buildWall (x, y) d Bottom  =
        Polygon [
          ( (x * d - (d/2) - wallWidth), (y * d - (d/2) + wallWidth) ),
          ( (x * d + (d/2) + wallWidth), (y * d - (d/2) + wallWidth) ),
          ( (x * d + (d/2) + wallWidth), (y * d - (d/2) - wallWidth) ),
          ( (x * d - (d/2) - wallWidth), (y * d - (d/2) - wallWidth) )
        ]
      buildWall (x, y) d Right   =
        Polygon [
          ( (x * d + (d/2)  + wallWidth), (y * d + (d/2) +wallWidth) ),
          ( (x * d + (d/2)  + wallWidth), (y * d - (d/2) -wallWidth) ),
          ( (x * d + (d/2) - wallWidth), (y * d - (d/2) -wallWidth) ),
          ( (x * d + (d/2) - wallWidth), (y * d + (d/2) +wallWidth) )
        ]
      {- drawTile
         PRE:
         POST:
         EXAMPLES:      drawTile ==
      -}
      drawTile :: (Position, Tile) -> Float -> [Picture]
      drawTile t d = [(drawLines (fst t) d), (drawPolygon (fst t) d)]
          where
            drawLines :: Position -> Float -> Picture
            drawLines p d = Line (drawSquare p d)
                -- Line [
                -- ((x * d - (d/2)), (y * d + (d/2))),
                -- ((x * d + (d/2)), (y * d + (d/2))),
                -- ((x * d + (d/2)), (y * d - (d/2))),
                -- ((x * d - (d/2)), (y * d - (d/2))),
                -- ((x * d - (d/2)), (y * d + (d/2)))
                -- ]

            drawPolygon :: Position -> Float -> Picture
            drawPolygon p d = color red $ Polygon (drawSquare p d)

            drawSquare :: Position -> Float -> [(Float, Float)]
            drawSquare (x, y) d = [
              ((x * d - (d/2)), (y * d + (d/2))),
              ((x * d + (d/2)), (y * d + (d/2))),
              ((x * d + (d/2)), (y * d - (d/2))),
              ((x * d - (d/2)), (y * d - (d/2))),
              ((x * d - (d/2)), (y * d + (d/2)))
              ]


{- renderMap m
   PRE:       elements in m are valid tiles.
   POST:      a window with the map drawn
   EXAMPLES:  generateWindow ==
-}
renderMap :: Board -> IO()
renderMap tiles = display (window 1000 1000) background (drawMap tiles tileDimensions (length t))
