module Core.Board.Actor
        ( Actor , Actors(..), Position
        , makeMove , position
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
data Actor = Player   Position Direction Direction
           | Computer Position Direction Paths
           deriving (Show)

data Actors = Actors Actor [Actor] deriving (Show)

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
createPlayer :: (Float, Float) -> Direction -> Direction -> Actor
createPlayer p d n = Player p d n

{- directions a
   PRE:       a must be Player.
   POST:      Direction and next direction of a.
   EXAMPLES:  direction ==
-}
directions :: Actor -> (Direction, Direction)
directions (Player _ a b) = (a, b)

-------------------------------
-- AI ONLY FUNCTIONS
-------------------------------

{- createAI c
   PRE:       True
   POST:      An AI with the start position c.
   EXAMPLES:  createAI  ==
-}
createAI :: (Float, Float) -> Direction -> Paths -> Actor
createAI p d ps = Computer p d ps

{- paths c
   PRE:       c must be computer
   POST:      Paths of c.
   EXAMPLES:  paths ==
-}
paths :: Actor -> Paths
paths (Computer _ _ p) = p

-------------------------------
-- COMMON FUNCTIONS
-------------------------------

{- position a
   PRE:       True
   POST:      The current position of a.
   EXAMPLES:  position (Player (5, -4)) == (5, -4)
-}
position :: Actor -> (Float, Float)
position (Player   a _ _) = a
position (Computer a _ _) = a

{- direction a
   PRE:       True
   POST:      The direction of a.
   EXAMPLES:  direction  ==
-}
direction :: Actor -> Direction
direction (Player   _ a _) = a
direction (Computer _ a _) = a

{- makeMove s a
   PRE:       True
   POST:      a with new position based on s
   EXAMPLES:  makeMove ==
-}
makeMove :: (Float, Float) -> Actor -> Actor
makeMove speed (Player   position direction n) = Player (position + direction * speed) direction n
makeMove speed (Computer position direction n) = Computer (position + direction * speed) direction n

{- isAI a
   PRE:           True
   POST:          True if a is an AI, otherwise False.
   EXAMPLES:      isAI  ==
-}
isAI :: Actor -> Bool
isAI (Computer _ _ _) = True
isAI _ = False
