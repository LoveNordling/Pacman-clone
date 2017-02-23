module Core.GraphicsEngine (render, tileSize) where

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
tileSize b = 19*round (sqrt ( fromIntegral (mapSize) / fromIntegral (length b) ))
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
render (GameState.State l s (Actor.Actors p c) t) = drawMap (Level.getBoard l) s p c t

{- drawMap t p c
   PRE:           True
   POST:          The image to be displayed, based on t with entities p and c.
   EXAMPLES:      drawMap ==
-}
drawMap :: Board.Board -> Int -> Actor.Actor -> [Actor.Actor] -> Float -> Picture
drawMap b s p cs time =
  let
    board      = elems b
    dimensions = tileSize b
    interior   = drawInterior board dimensions []
    character  = [drawActor dimensions time p]
    computer   = map (drawActor dimensions time) cs
    scoreboard = [drawScoreboard dimensions s]
  in
    Pictures (interior ++ character ++ computer ++ scoreboard)
      where
        {- drawScoreboard s
           PRE:       True
           POST:      ...
           EXAMPLES:  drawScoreboard  ==
        -}
        drawScoreboard :: Int -> Int -> Picture
        drawScoreboard d s = translateAndColor (-9 :: Float, -4 :: Float) d red (Scale 0.3 0.3 (Text ("Score: " ++ (show s))))
        {- drawActor a f
           PRE:       True
           POST:      The actors a to be displayed.
           EXAMPLES:  drawActor ==
        -}
        drawActor :: Int -> Float -> Actor.Actor -> Picture
        drawActor d time a
          | Actor.isAI a = translateAndColor p d green (circle $ fromIntegral d / 2)
          | otherwise    = translateAndColor p d blue (Scale 0.1 0.1 s)
          where
            p = Actor.position a
            n = Actor.direction a
            t = round time
            s = Actor.getPicture a
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
        -}
        makeCircle :: (Int, Int) -> Int -> Color -> Picture
        makeCircle p d c = translateAndColor p d c (circleSolid (fromIntegral (d `div` 3)))
        {- translateAndColor p d c s
           PRE:       p must be valid coordinates
           POST:      Shape s positioned based on p and d with color c.
           EXAMPLES:  translateAndColor ==
           EXAMPLES:  makeCircle ==
        -}
        translateAndColor :: (Common.Position a) => (a, a) -> Int -> Color -> Picture -> Picture
        translateAndColor p d c =
          let
            rowLength = fromIntegral (round (sqrt (fromIntegral (length b))))
          in
            (Common.setCoordinate p (fromIntegral d) rowLength) . (color c)
