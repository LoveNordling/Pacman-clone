module Core.Extras.Resources
        ( loadSprites, playerSprites, aiSprites )
where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.IO.Unsafe (unsafePerformIO)

import qualified Core.Board.Actor as Actor

dazzleManClosedRight, dazzleManClosedLeft, dazzleManRight, dazzleManLeft :: String
dazzleManClosedRight = "res/dazzleManClosedRight.png" -- unused
dazzleManClosedLeft = "res/dazzleManClosedLeft.png" -- unused
dazzleManRight = "res/dazzleManRight.png"
dazzleManLeft  = "res/dazzleManLeft.png"

monsterLeft, monsterRight :: String
monsterLeft  = "res/monsterLeft.png"
monsterRight = "res/monsterRight.png"

-- The player sprites
playerSprites :: [Actor.Sprite]
playerSprites = loadSprites [ ((-1,0), dazzleManLeft), ((1,0), dazzleManRight) ]

aiSprites :: [Actor.Sprite]
aiSprites = loadSprites [ ((-1,0), monsterLeft), ((1,0), monsterRight) ]

{- loadSprites s
   PRE:       ...
   POST:      ...
   EXAMPLES:  loadSprites ==
   VARIANT:   |s|
-}
loadSprites :: [((Float, Float), String)] -> [Actor.Sprite]
loadSprites []         = []
loadSprites ((d,s):xs) = (Actor.Sprite (loadImage s) d):(loadSprites xs)
  where
  -- Borrowed from Gloss.Game
  {- loadImage p
     PRE:       p must be a valid path to a PNG image.
     POST:      File at path p loaded as a picture.
     EXAMPLES:  loadImage ==
  -}
  loadImage :: String -> Picture
  loadImage path = fromMaybe (text "Could not load image.") (unsafePerformIO $ loadJuicyPNG path)
