module Graphic where
import Graphics.Gloss
import Tile
import Prelude hiding(Right, Left)

{- window x y
   PRE:           x and y > 0
   POST:          a window with size (x, y)
   EXAMPLES:      ...
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

{- functionIdentifier arguments
   PRE:           n > 0,
   POST:          ...
   EXAMPLES:      functionIdentifier ==
-}
tileDimensions :: Int -> Float
tileDimensions n = 10 * sqrt ( fromIntegral (boardDimensions) / fromIntegral (n) )

{- createBoard t
   PRE:           ...
   POST:          ...
   EXAMPLES:      createBoard ==
   VARIANT:       |t|
-}
drawMap :: [((Float, Float), Tile)] -> Float -> Picture
drawMap t d = Pictures (drawMapAux t [] []) where
  drawMapAux :: [((Float, Float), Tile)] -> [Picture] -> [Picture] -> [Picture]
  drawMapAux [] pAcc wAcc = (pAcc ++ wAcc)
  drawMapAux (t:ts) pAcc wAcc =
    let
      w = drawWall t d
      x = drawPicture t d
    in
      drawMapAux ts (x ++ pAcc) (w ++ wAcc)


{- drawWall arguments
     PRE:           pre-condition on the arguments
     POST:          post-condition on the result, in terms of the arguments
     SIDE EFFECTS:  if any, including exceptions
     EXAMPLES:      drawWall ==
     VARIANT:       None
  -}
drawWall :: ((Float, Float), Tile) -> Float -> [Picture]
drawWall ((x, y), (Tile (True, l, b, r) a)) d = (buildWall (x, y) d Top):(drawWall ((x, y), Tile (False, l, b, r) a) d)
drawWall ((x, y), (Tile (_, True, b, r) a)) d = (buildWall (x, y) d Left):(drawWall ((x, y), Tile (False, False, b, r) a) d)

drawWall ((x, y), (Tile (_, _, True, r) a)) d = (buildWall (x, y) d Bottom):(drawWall ((x, y), Tile (False, False, False, r) a) d)
drawWall ((x, y), (Tile (_, _, _, True) a)) d = (buildWall (x, y) d Right):(drawWall ((x, y), Tile (False, False, False, False) a) d)
drawWall ((x, y), (Tile (_, _, _, _) a)) d = []

buildWall :: (Float, Float) -> Float -> BorderDirection -> Picture
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


drawPicture :: ((Float, Float), Tile) -> Float -> [Picture]
drawPicture t d =
  let
    l = drawLines (fst t) d
    p = drawPolygon (fst t) d
  in
    [p, l]

drawLines :: (Float, Float) -> Float -> Picture
drawLines (x, y) d =
    Line [
    ((x * d - (d/2)), (y * d + (d/2))),
    ((x * d + (d/2)), (y * d + (d/2))),
    ((x * d + (d/2)), (y * d - (d/2))),
    ((x * d - (d/2)), (y * d - (d/2))),
    ((x * d - (d/2)), (y * d + (d/2)))
    ]

drawPolygon :: (Float, Float) -> Float -> Picture
drawPolygon (x, y) d =
    let
      c = red
    in
      color c $ Polygon [
      ((x * d - (d/2)), (y * d + (d/2))),
      ((x * d + (d/2)), (y * d + (d/2))),
      ((x * d + (d/2)), (y * d - (d/2))),
      ((x * d - (d/2)), (y * d - (d/2))),
      ((x * d - (d/2)), (y * d + (d/2)))
      ]

data BorderDirection = Top | Left | Bottom | Right


wallWidth :: Float
wallWidth = 10


{- drawSquare
   PRE:           True
   POST:          post-condition on the result, in terms of the arguments
   SIDE EFFECTS:  None
   EXAMPLES:      drawSquare  ==
   VARIANT:       None
-}
--drawTile :: Picture
--drawTile = undefined

{- generateWindow
   PRE:           ...
   POST:          ...
   EXAMPLES:      generateWindow ==
-}
renderMap :: [((Float, Float), Tile)] -> IO()
renderMap tiles = display (window 1000 1000) background (drawMap tiles (tileDimensions (length tiles)))
