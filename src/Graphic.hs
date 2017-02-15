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

-- Suppress HLint messages
{-# ANN module "HLint: Ignore Use mappend" #-}

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

-- Color of the board
background :: Color
background = white

-- Dimensions of the board
boardDimensions :: Int
boardDimensions = 1000

{- window x y
   PRE:           x and y must be bigger than 0
   POST:          a window with width x and height y pixels.
   EXAMPLES:      window 1000 1000 gives a window with height and width 1000 pixels.
-}
window :: Int -> Int -> Display
window x y = InWindow "Dazzlebox" (x, y) (0, 0)

{- tileDimensions n
   PRE:           n > 0
   POST:          the calculated width and height of n
   EXAMPLES:      tileDimensions 9 == 74,5
-}
tileDimensions :: Int -> Float
tileDimensions n = 10 * sqrt ( fromIntegral (boardDimensions) / fromIntegral (n) )

{- drawMap b d
   PRE:       b is a valid board
   POST:      the game map based on b.
   EXAMPLES:  createBoard ==
-}
drawMap :: Board -> Float -> Picture
drawMap b dimension = Pictures (generateMapElements b ([], []))
    where
      {- generateMapElements b acc
         PRE:           True
         POST:          Display elements acc based on board b.
         EXAMPLES:      generateMapElements ==
         VARIANT:       |b|
      -}
      generateMapElements :: Board -> ([Picture], [Picture]) -> [Picture]
      generateMapElements [] (floors, walls) = floors ++ walls
      generateMapElements (x@(p, t):ts) (f, w) = generateMapElements ts (drawFloor p ++ f, (drawWall x []) ++ w)
        -- where
        {- drawFloor p
           PRE:           p must be a valid tile position
           POST:          A floor tile on position p
           EXAMPLES:      drawFloor  ==
        -}
      drawFloor :: Position -> [Picture]
      drawFloor p = let square = drawSquare p in [ Line square, color red $ Polygon square ]
        where
          {- drawSquare (x, y)
             PRE:           True
             POST:          coordinates for a square based on (x, y)
             EXAMPLES:      drawSquare ==
          -}
          drawSquare :: Position -> [Position]
          drawSquare (x, y) = [
            ((x * dimension - (dimension/2)), (y * dimension + (dimension/2))),
            ((x * dimension + (dimension/2)), (y * dimension + (dimension/2))),
            ((x * dimension + (dimension/2)), (y * dimension - (dimension/2))),
            ((x * dimension - (dimension/2)), (y * dimension - (dimension/2))),
            ((x * dimension - (dimension/2)), (y * dimension + (dimension/2)))
            ]
      {- drawWall (p, t)
         PRE:           True
         POST:          Walls for t based on t and p
         EXAMPLES:      drawWall ==
         VARIANT:       ???
      -}
      drawWall :: (Position, Tile) -> [Picture] -> [Picture]
      drawWall (p, (Tile (top, left, bottom, right) a)) acc
        | top       = drawWall (p, Tile (False, left, bottom, right)  a) ((buildWall p Top):acc)
        | left      = drawWall (p, Tile (False, False, bottom, right) a) ((buildWall p Left):acc)
        | bottom    = drawWall (p, Tile (False, False, False, right)  a) ((buildWall p Bottom):acc)
        | right     = drawWall (p, Tile (False, False, False, False)  a) ((buildWall p Right):acc)
        | otherwise = acc
        where
          {- buildWall p
             PRE:           True
             POST:          Polygon shape based on p.
             EXAMPLES:      buildWall ==
          -}
          buildWall :: Position -> BorderDirection -> Picture
          buildWall (x, y) Top =
            Polygon [
              ( (x * dimension - (dimension/2) - wallWidth), (y * dimension + (dimension/2) + wallWidth) ),
              ( (x * dimension + (dimension/2) + wallWidth), (y * dimension + (dimension/2) + wallWidth) ),
              ( (x * dimension + (dimension/2) + wallWidth), (y * dimension + (dimension/2) - wallWidth) ),
              ( (x * dimension - (dimension/2) - wallWidth), (y * dimension + (dimension/2) - wallWidth) )
            ]
          buildWall (x, y) Left =
            Polygon [
              ( (x * dimension - (dimension/2) + wallWidth), (y * dimension + (dimension/2) + wallWidth) ),
              ( (x * dimension - (dimension/2) + wallWidth), (y * dimension - (dimension/2) - wallWidth) ),
              ( (x * dimension - (dimension/2) - wallWidth), (y * dimension - (dimension/2) - wallWidth) ),
              ( (x * dimension - (dimension/2) - wallWidth), (y * dimension + (dimension/2) + wallWidth) )
            ]
          buildWall (x, y) Bottom  =
            Polygon [
              ( (x * dimension - (dimension/2) - wallWidth), (y * dimension - (dimension/2) + wallWidth) ),
              ( (x * dimension + (dimension/2) + wallWidth), (y * dimension - (dimension/2) + wallWidth) ),
              ( (x * dimension + (dimension/2) + wallWidth), (y * dimension - (dimension/2) - wallWidth) ),
              ( (x * dimension - (dimension/2) - wallWidth), (y * dimension - (dimension/2) - wallWidth) )
            ]
          buildWall (x, y) Right   =
            Polygon [
              ( (x * dimension + (dimension/2) + wallWidth), (y * dimension + (dimension/2) + wallWidth) ),
              ( (x * dimension + (dimension/2) + wallWidth), (y * dimension - (dimension/2) - wallWidth) ),
              ( (x * dimension + (dimension/2) - wallWidth), (y * dimension - (dimension/2) - wallWidth) ),
              ( (x * dimension + (dimension/2) - wallWidth), (y * dimension + (dimension/2) + wallWidth) )
            ]

{- renderMap m
   PRE:       elements in m are valid tiles.
   POST:      a window with the map drawn
   EXAMPLES:  generateWindow ==
-}
renderMap :: Board -> IO()
renderMap tiles = display (window 1000 1000) background (drawMap tiles (tileDimensions (length tiles)))
