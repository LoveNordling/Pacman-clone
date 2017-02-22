module Core.GraphicsEngine (render, tileSize) where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe (unsafePerformIO)

import Core.Extras.Common
import qualified Core.Board.GameState as GameState
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Board as Board
import qualified Core.Board.Level as Level
import qualified Core.Board.Tile as Tile

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
render :: GameState.GameState -> Picture
render (GameState.State l s (Actor.Actors p c) _) = drawMap (Level.getBoard l) s p c

{- drawMap t p c
   PRE:           True
   POST:          The image to be displayed, based on t with entities p and c.
   EXAMPLES:      drawMap ==
-}
drawMap :: Board.Board -> Int -> Actor.Actor -> [Actor.Actor] -> Picture
drawMap b s p cs =
  let
    board      = elems b
    dimensions = tileSize (length board)
    interior   = drawInterior board dimensions []
    character  = [drawActor dimensions p]
    computer   = map (drawActor dimensions) cs --drawComputers cs
    scoreboard = drawScoreboard s dimensions
  in
    Pictures (interior ++ character ++ computer ++ [scoreboard])
      where
        {- drawScoreboard s
           PRE:       True
           POST:      ...
           EXAMPLES:  drawScoreboard  ==
        -}
        drawScoreboard :: Int -> Int -> Picture
        drawScoreboard s d = translateAndColor (-500 :: Float, 500 :: Float) d black (Text ("Score: " ++ (show s)))
        {- drawActor a f
           PRE:       True
           POST:      The actors a to be displayed.
           EXAMPLES:  drawActor ==
        -}
        drawActor :: Int -> Actor.Actor -> Picture
        drawActor d a
          | Actor.isAI a = translateAndColor p d green (circle $ fromIntegral d / 2)
          | otherwise    = translateAndColor p d blue (Scale 0.1 0.1 (loadImage "res/dazzleManRight.png"))
          where
            p = Actor.position a
        {- drawInterior t f acc
           PRE:       True
           POST:      The interiors in t to be displayed.
           EXAMPLES:  drawInterior ==
           VARIANT:   |t|
        -}
        drawInterior :: Board.Tiles -> Int -> [Picture] -> [Picture]
        drawInterior []             d acc = acc
        drawInterior ((Tile.Floor p True):ts) d acc = drawInterior ts d ((makeRectangle p d red):(makeCircle p d yellow):acc)
        drawInterior ((Tile.Floor p _):ts) d acc = drawInterior ts d ((makeRectangle p d red):acc)
        drawInterior ((Tile.Wall p):ts)  d acc = drawInterior ts d ((makeRectangle p d black):acc)
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

-- Borrowed from Gloss.Game
{- loadImage p
   PRE:       p must be a valid path to a PNG image.
   POST:      File at path p loaded as a picture.
   EXAMPLES:  loadImage ==
-}
loadImage :: String -> Picture
loadImage path = maybe (text "Could not load image.") id (unsafePerformIO $ loadJuicyPNG path)
