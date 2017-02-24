module Core.Extras.Sprite
        ( Sprite(..), Sprites , player, ai, load, testSuite ) where

import Test.HUnit
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.IO.Unsafe (unsafePerformIO)
import qualified Core.Extras.Resources as Resources

{-
  REPRESENTATION CONVENTION:
     Sprite represents a game character sprite and is given by Sprite p d where p is the image to show and d is the direction it should be associated with.

  REPRESENTATION INVARIANT:
     The direction must be either (-1,0) or (1,0).
-}
data Sprite = Sprite Picture (Float, Float)
              deriving (Eq, Show)

-- A list of sprites
type Sprites = [Sprite]

-- The player sprites
player :: Sprites
player = load [ ((-1,0), Resources.dazzleManLeft), ((1,0), Resources.dazzleManRight) ]

-- The AI sprites
ai :: Sprites
ai = load [ ((-1,0), Resources.monsterLeft), ((1,0), Resources.monsterRight) ]

{- loadSprites s
   PRE:           Second component of each element in s must be a valid path to a PNG image
   POST:          Sprites with bitmap images loaded from the second component of each element in s, associated with the first component
   SIDE EFFECTS:  None
   EXAMPLES:      loadSprites load [ ((-1,0), "path/to/file") ] gives a list of sprites with only one element.
   VARIANT:       |s|
-}
load :: [((Float, Float), String)] -> Sprites
load []         = []
load ((d,s):xs) = ((Sprite (loadImage s) d)):(load xs)
  where
    {- loadImage p
       PRE:           p must be a valid path to a PNG image
       POST:          File at path p loaded as a bitmap image
       SIDE EFFECTS:  None
       EXAMPLES:      loadImage "path/to/file.png" loads file.png as a picture.
    -}
    loadImage :: String -> Picture
    loadImage path = fromMaybe (text "Could not load image.") (unsafePerformIO $ loadJuicyPNG path) -- Borrowed from Gloss.Game

-------------------------------------------
-- TEST CASES
-------------------------------------------
test1 :: Test
testSuite = TestList [ test1, test2 ]

-- load
test1 =
  let
    s = length player
    r = [((-1,0), Resources.dazzleManLeft), ((1,0), Resources.dazzleManRight)]
  in
    TestLabel "Load Test #1" .
      TestCase $ assertEqual "" (s) (length (load r))

test2 = TestLabel "Load Test #1" .
          TestCase $ assertEqual "" ([]) (load [])
