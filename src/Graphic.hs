module Graphic where
import Graphics.Gloss

{- window x y
   PRE:           x and y > 0
   POST:          a window with size (x, y)
   EXAMPLES:      ...
-}
window :: Int -> Int -> Display
window x y = undefined

{- background
   PRE:           True
   POST:          a color
   EXAMPLES:      background ==
-}
background :: Color
background = undefined

{- createBoard
   PRE:           ...
   POST:          ...
   EXAMPLES:      createBoard ==
   VARIANT:       ...
-}
createBoard :: Picture
createBoard = undefined

{- generateWindow
   PRE:           ...
   POST:          ...
   EXAMPLES:      generateWindow ==
-}
generateWindow :: IO()
generateWindow = undefined
