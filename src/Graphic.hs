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

getX x d = x * d - (d/2)
getY y d = y * d - (d/2)

drawMap :: [Tile] -> Picture
drawMap t = Pictures (generateMapElements t (tileDimensions (length t)) [] [])
  where
      {- generateMapElements arguments
         PRE:           True
         POST:          post-condition on the result, in terms of the arguments
         SIDE EFFECTS:  None
         EXAMPLES:      generateMapElements  ==
         VARIANT:       None
      -}
      generateMapElements :: [Tile] -> Float -> [Picture] -> [Picture] -> [Picture]
      generateMapElements []                d floors walls = floors ++ walls
      generateMapElements ((Tile p b _):ts) d floors walls = --generateMapElements ts d ((drawFloor t):acc) ((drawWall ))
        let f = ((drawFloor p):floors)
            w = ((drawWalls p b []) ++ walls)
        in  generateMapElements ts d f w
            where
            {- drawFloor arguments
               PRE:           pre-condition on the arguments
               POST:          post-condition on the result, in terms of the arguments
               SIDE EFFECTS:  if any, including exceptions
               EXAMPLES:      drawFloor ==
               VARIANT:       None
            -}
            drawFloor :: (Float, Float) -> Picture
            drawFloor (x, y) = translate (getX x d) (getY y d) $ color red $ rectangleSolid d d
            {- drawWall arguments
               PRE:           pre-condition on the arguments
               POST:          post-condition on the result, in terms of the arguments
               SIDE EFFECTS:  if any, including exceptions
               EXAMPLES:      drawWall ==
               VARIANT:       None
            -}
            drawWalls :: (Float, Float) -> (Bool, Bool, Bool, Bool) -> [Picture] -> [Picture]
            drawWalls (x, y) (top, left, bottom, right) acc
              | top       = drawWalls (x, y) (False, left, bottom, right) ((buildWall (x, y) Top):acc)
              | left      = drawWalls (x, y) (False, False, bottom, right) ((buildWall (x, y) Left):acc)
              | bottom    = drawWalls (x, y) (False, False, False, right)  ((buildWall (x, y) Bottom):acc)
              | right     = drawWalls (x, y) (False, False, False, False)  ((buildWall (x, y) Right):acc)
              | otherwise = acc
              where
                buildWall :: (Float, Float) -> BorderDirection -> Picture
                buildWall (x, y) Top    = translate (getX x d) ((getY y d) + (d/2)) $ color black $ rectangleSolid d wallWidth
                buildWall (x, y) Left   = translate (getX x d - (d/2)) ((getY y d)) $ color black $ rectangleSolid wallWidth d
                buildWall (x, y) Bottom = translate (getX x d) ((getY y d) - (d/2)) $ color black $ rectangleSolid d wallWidth
                buildWall (x, y) Right  = translate (getX x d + (d/2)) ((getY y d)) $ color black $ rectangleSolid wallWidth d

{- renderMap m
   PRE:       elements in m are valid tiles.
   POST:      a window with the map drawn
   EXAMPLES:  generateWindow ==
-}
renderMap :: [Tile] -> IO()
renderMap b = display (window 1000 1000) background (drawMap b)
