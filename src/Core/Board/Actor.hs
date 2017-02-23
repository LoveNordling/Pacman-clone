module Core.Board.Actor
        ( Actor , Actors(..), Sprite(..), Position
        , makeMove , position, getPicture, sprites
        , createAI , createPlayer
        , directions , direction
        , paths, isAI )
where
import Graphics.Gloss

{-
  REPRESENTATION CONVENTION:
    A player character is given by Player (x, y) (a, b) s  where (x, y) is the initial x and y coordinates of the player, (a, b) is initial direction of the player and s is the score.
    A computer AI is given by Computer (c, d) (e, f) p where (c, d) is initial the x and y coordinates of the AI, (e, f) is the initial direction of the AI and p is the nearest path to the player.

  REPRESENTATION INVARIANT:
    The coordinates of the player and computer must not be out of bounds compared to the maps they are occupying. The components of the direction of the player and computer must be between -1 and 1.
-}
data Actor = Player   Position Direction Direction [Sprite]
           | Computer Position Direction Paths [Sprite]

 {-
   REPRESENTATION CONVENTION:
      Actors represents all the actors on the board. Actors is given by a s, where a is an Actor and s is a list of Actors.

   REPRESENTATION INVARIANT:
      Second argument must be a Player.
 -}
data Actors = Actors Actor [Actor]

data Sprite = Sprite Picture Direction


-- Represents the score of a player.
type Score = Int

-- Paths of the AI
type Paths = [(Int, Int)]

-- First component is horizontal movement, second is vertical movement.
type Direction = (Float, Float)

type Position = (Float, Float)


-------------------------------
-- PLAYER ONLY FUNCTIONS
-------------------------------

{- createPlayer p d n
   PRE:       True
   POST:      A Player with position p, direction d, next direction n.
   EXAMPLES:  createPlayer ==
-}
createPlayer :: (Float, Float) -> Direction -> Direction -> [Sprite] -> Actor
createPlayer p d n s = Player p d n s

{- directions a
   PRE:       a must be Player.
   POST:      Direction and next direction of a.
   EXAMPLES:  direction ==
-}
directions :: Actor -> (Direction, Direction)
directions (Player _ a b _) = (a, b)

-------------------------------
-- AI ONLY FUNCTIONS
-------------------------------

{- createAI c
   PRE:       True
   POST:      An AI with the start position c.
   EXAMPLES:  createAI  ==
-}
createAI :: (Float, Float) -> Direction -> Paths -> [Sprite] -> Actor
createAI position direction paths sprites = Computer position direction paths sprites

{- paths c
   PRE:       c must be computer
   POST:      Paths of c.
   EXAMPLES:  paths ==
-}
paths :: Actor -> Paths
paths (Computer _ _ p _) = p

-------------------------------
-- COMMON FUNCTIONS
-------------------------------

{- position a
   PRE:       True
   POST:      The current position of a.
   EXAMPLES:  position (Player (5, -4)) == (5, -4)
-}
position :: Actor -> (Float, Float)
position (Player   a _ _ _) = a
position (Computer a _ _ _) = a

{- direction a
   PRE:       True
   POST:      The direction of a.
   EXAMPLES:  direction  ==
-}
direction :: Actor -> Direction
direction (Player   _ a _ _) = a
direction (Computer _ a _ _) = a

{- makeMove s a
   PRE:       True
   POST:      a with new position based on s
   EXAMPLES:  makeMove ==
-}
makeMove :: (Float, Float) -> Actor -> Actor
makeMove speed (Player   position direction n s) = Player   (position + direction * speed) direction n s
makeMove speed (Computer position direction n s) = Computer (position + direction * speed) direction n s

{- isAI a
   PRE:       True
   POST:      True if a is an AI, otherwise False.
   EXAMPLES:  isAI  ==
-}
isAI :: Actor -> Bool
isAI (Computer _ _ _ _) = True
isAI _ = False

{- sprites a
   PRE:       True
   POST:      Sprites of a.
   EXAMPLES:  sprites ==
-}
sprites :: Actor -> [Sprite]
sprites (Player _ _ _ s)   = s
sprites (Computer _ _ _ s) = s

{- getPicture a d
   PRE:       a must have at least one sprite.
   POST:      Image from a with associated with d.
   EXAMPLES:  getPicture  ==
-}

-- getPicture (Player _ (0,0) d     s)      = pictureFromDirection s d
-- getPicture (Player _ d _ s)              = pictureFromDirection s d

getPicture :: Actor -> Picture
getPicture (Player _ (0,0) (0,0) ((Sprite s _):_)) = s
getPicture a = pictureFromDirection (sprites a) (direction a)

{- pictureFromDirection s d
   PRE:       s must be non-empty.
   POST:      Element in s associated with d.
   EXAMPLES:  pictureFromDirection ==
   VARIANT:   |s|
-}
pictureFromDirection :: [Sprite] -> Direction -> Picture
pictureFromDirection ((Sprite s x):xs) d
  | length xs == 0 || d == x || d == (0,0) = s
  | otherwise = pictureFromDirection xs d
