module Core.Extras.Sprite
        ( Sprite(..), Sprites
        , player, ai, load )
where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.IO.Unsafe (unsafePerformIO)

import qualified Core.Extras.Resources as Resources

{-
  REPRESENTATION CONVENTION:
     Sprite represents a game character sprite and is given by Sprite p d where p is the image to show and d for which direction it should be shown.

  REPRESENTATION INVARIANT:
     The direction must be either (-1,0) or (1,0).
-}
data Sprite = Sprite Picture (Float, Float)

-- A list of sprites
type Sprites = [Sprite]

-- The player sprites
player :: Sprites
player = load [ ((-1,0), Resources.dazzleManLeft), ((1,0), Resources.dazzleManRight) ]

-- The AI sprites
ai :: Sprites
ai = load [ ((-1,0), Resources.monsterLeft), ((1,0), Resources.monsterRight) ]

{- loadSprites s
   PRE:       ...
   POST:      ...
   EXAMPLES:  loadSprites ==
   VARIANT:   |s|
-}
load :: [((Float, Float), String)] -> Sprites
load []         = []
load ((d,s):xs) = ((Sprite (loadImage s) d)):(load xs)
  where
    {- loadImage p
       PRE:       p must be a valid path to a PNG image.
       POST:      File at path p loaded as a picture.
       EXAMPLES:  loadImage ==
    -}
    loadImage :: String -> Picture
    loadImage path = fromMaybe (text "Could not load image.") (unsafePerformIO $ loadJuicyPNG path) -- Borrowed from Gloss.Game
