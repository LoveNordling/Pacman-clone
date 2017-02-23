module Core.Extras.Resources
        ( getSprite )
where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.IO.Unsafe (unsafePerformIO)


{- getSprite p b
   PRE:       p can only be ...?
   POST:
   EXAMPLES:  getSprite ==
-}
getSprite :: (Float, Float) -> Bool -> Picture
getSprite (-1, 0) mode = loadImage (if mode then dazzleManClosedLeft else dazzleManLeft)
getSprite (0,  1) mode = Rotate (-90) $ loadImage (if mode then dazzleManClosedRight else dazzleManRight)
getSprite (0, -1) mode = Rotate (90) $ loadImage (if mode then dazzleManClosedRight else dazzleManRight)
getSprite _       mode = loadImage (if mode then dazzleManClosedRight else dazzleManRight)

dazzleManClosedRight, dazzleManClosedLeft, dazzleManRight, dazzleManLeft :: String
dazzleManClosedRight = "res/dazzleManClosedRight.png"
dazzleManClosedLeft = "res/dazzleManClosedLeft.png"
dazzleManRight = "res/dazzleManRight.png"
dazzleManLeft = "res/dazzleManLeft.png"

-- Borrowed from Gloss.Game
{- loadImage p
   PRE:       p must be a valid path to a PNG image.
   POST:      File at path p loaded as a picture.
   EXAMPLES:  loadImage ==
-}
loadImage :: String -> Picture
loadImage path = fromMaybe (text "Could not load image.") (unsafePerformIO $ loadJuicyPNG path)
