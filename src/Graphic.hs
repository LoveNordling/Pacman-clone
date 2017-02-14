module Graphic(generateWindow) where
import Board
import Graphics.Gloss

{- window x y
   PRE:           x and y > 0
   POST:          a window with size (x, y)
   EXAMPLES:      window 1000 1000 gives a window with width and height 1000.
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

-- 499x355

board :: Picture
board = Pictures [
  Line [
    (-480, -300), (-480, -250),
    (-430, -250), (-430, -300),
    (-480, -300),

    (-480, -250), (-480, -200),
    (-430, -200), (-430, -250),
    (-480, -250)
  ],
  Line [
    (-430, -300), (-380, -300),
    (-380, -250), (-430, -250),
    (-430, -200),

    (-430, -250), (-380, -250),
    (-380, -200), (-430, -200),
    (-430, -250)
  ],

  Line [(499,-335), (0,-335)]
 ]

createBoard :: [Tile] -> Picture
createBoard (t:ts) = undefined

generateWindow :: [Tile] -> IO()
generateWindow tiles = display (window 1000 1000) background board--(createBoard tiles)
