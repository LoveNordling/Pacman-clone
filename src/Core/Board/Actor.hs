module Core.Board.Actor
        ( Actor , Actors(..), Position, Direction
        , makeMove , position, getPicture, sprites
        , createAI , createPlayer
        , directions , direction
        , paths, isAI, testSuite )
where

import Test.HUnit
import Graphics.Gloss

import qualified Core.Extras.Sprite as Sprite

{-
  REPRESENTATION CONVENTION:
    A player character is given by Player (x, y) d nd s where (x, y) is the x and y coordinates of the player, d is the direction of the player, nd is the desired direction of the player and s is a list of sprites for the player.

    A computer AI is given by Computer (c, d) d p s where (c, d) is the x and y coordinates of the AI, d is the direction of the AI and p is the nearest path to the player and s is a list of sprites for the AI.

  REPRESENTATION INVARIANT:
    The coordinates of the player and computer must not be out of bounds compared to the maps they are occupying. The directions of the Player and Computer must be (-1,0), (0,-1), (0,0), (1,0) or (0,1).
-}
data Actor = Player   Position Direction Direction Sprite.Sprites
           | Computer Position Direction Paths Sprite.Sprites
           deriving (Eq, Show)

 {-
   REPRESENTATION CONVENTION:
      Actors represents all the actors on the board. Actors is given by a s, where a is an Actor and s is a list of Actors.

   REPRESENTATION INVARIANT:
      The first Actor must be a Player. The list of Actor must be Computers.
 -}
data Actors = Actors Actor [Actor] deriving (Eq, Show)

-- Paths of the AI
type Paths = [(Int, Int)]

-- Direction of Actor.
-- First component is horizontal movement, second is vertical movement.
type Direction = (Float, Float)

-- The position of the actor
type Position = (Float, Float)

-------------------------------
-- PLAYER ONLY FUNCTIONS
-------------------------------

{- createPlayer p d n
   PRE:       True.
   POST:      A Player with position p, direction d, next direction n.
   EXAMPLES:  createPlayer (0,0) (0,0) (0,0) Sprite.player == Player (0,0) (0,0) (0,0) Sprite.player
-}
createPlayer :: (Float, Float) -> Direction -> Direction -> Sprite.Sprites -> Actor
createPlayer p d n s = Player p d n s

{- directions a
   PRE:           a must be Player
   POST:          Direction and next direction of a
   SIDE EFFECTS:  None
   EXAMPLES:      directions (createPlayer (20,21) (1,0) (0,1) Sprite.player) == ((1,0), (0,1))
                  directions (createAI (1,0) (2,4) [] Sprite.ai)              == ((0,0), (0,0))
-}
directions :: Actor -> (Direction, Direction)
directions (Player _ a b _) = (a, b)
directions _ = ((0,0), (0,0))

-------------------------------
-- AI ONLY FUNCTIONS
-------------------------------

{- createAI c d p s
   PRE:           True
   POST:          AI with the start position c, direction d, paths p and sprites s
   SIDE EFFECTS:  None
   EXAMPLES:      createAI (10,10) (0,1) [] Sprite.ai gives a Computer with position (10, 10), direction (0,1), an empty paths list and the sprites Sprite.ai.
-}
createAI :: (Float, Float) -> Direction -> Paths -> Sprite.Sprites -> Actor
createAI position direction paths sprites = Computer position direction paths sprites

{- paths c
   PRE:           c is computer
   POST:          Paths of c
   SIDE EFFECTS:  None
   EXAMPLES:      paths (Computer (0,0) (0,0) [(1,2), (3,4)] Sprite.ai == [(1,2), (3,4)]
                  paths (Player (20,10) (0,2) (0,1) Sprite.player)     == []
-}
paths :: Actor -> Paths
paths (Computer _ _ p _) = p
paths _ = []

-------------------------------
-- COMMON FUNCTIONS
-------------------------------

{- position a
   PRE:           True
   POST:          Position of a
   SIDE EFFECTS:  None
   EXAMPLES:      position (Player (5, -4) (1, 0) (0, 0) Sprite.player) == (5, -4)
-}
position :: Actor -> (Float, Float)
position (Player   a _ _ _) = a
position (Computer a _ _ _) = a

{- direction a
   PRE:           True
   POST:          The direction of a.
   SIDE EFFECTS:  None
   EXAMPLES:      direction (Player (5, -4) (1, 0) (0, 0) Sprite.player) == (1, 0)
-}
direction :: Actor -> Direction
direction (Player   _ a _ _) = a
direction (Computer _ a _ _) = a

{- makeMove s a
   PRE:           s > 0
   POST:          a with new position based on s and the direction of a.
   SIDE EFFECTS:  None
   EXAMPLES:      position (makeMove (2.4, 2.4) (Player (5, -4) (1, 0) (0, 0) [])) == (7.4, -4.0)
-}
makeMove :: (Float, Float) -> Actor -> Actor
makeMove speed (Player   position direction n s) = Player   (position + direction * speed) direction n s
makeMove speed (Computer position direction n s) = Computer (position + direction * speed) direction n s

{- isAI a
   PRE:           True
   POST:          True if a is an AI, otherwise False
   SIDE EFFECTS:  None
   EXAMPLES:      isAI (Computer (0,0) (0,0) [] []) == True
-}
isAI :: Actor -> Bool
isAI (Computer _ _ _ _) = True
isAI _ = False

{- sprites a
   PRE:           True
   POST:          Sprites of a
   SIDE EFFECTS:  None
   EXAMPLES:      sprites (Computer (0,0) (0,0) Sprite.ai) gives the sprites of the inputed Computer.
-}
sprites :: Actor -> Sprite.Sprites
sprites (Player _ _ _ s)   = s
sprites (Computer _ _ _ s) = s

{- getPicture a d
   PRE:           a must have at least one sprite
   POST:          Image from a that is associated with d
   SIDE EFFECTS:  None
   EXAMPLES:      getPicture (Player (0,0) (1, 0) (0,0) Sprite.player) gives the sprite associated with (1, 0)
                  getPicture (Player (0,0) (-1,0) (0,0) Sprite.player) gives the sprite associated with (-1,0)
-}
getPicture :: Actor -> Picture
getPicture (Player _ (0,0) (0,0) s) = pictureFromDirection s (1,0) -- default
getPicture a = pictureFromDirection (sprites a) (direction a)

{- pictureFromDirection s d
   PRE:           s must be non-empty
   POST:          Element in s associated with d
   SIDE EFFECTS:  None
   EXAMPLES:      pictureFromDirection Sprite.player (1,0) gives the image object associated with (1, 0)
   VARIANT:       |s|
-}
pictureFromDirection :: Sprite.Sprites -> Direction -> Picture
pictureFromDirection ((Sprite.Sprite s x):xs) d
  | null xs || d == x = s
  | otherwise = pictureFromDirection xs d

-------------------------------------------
-- TEST CASES
-------------------------------------------
test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13 :: Test
testSuite = TestList [ test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13 ]

-- createPlayer
test1 =
  let
    p = (0,0)
    s = Sprite.player
  in
    TestLabel "Create Player Test #1" .
      TestCase $ assertEqual "" (Player p p p s) (createPlayer p p p s)
-- Directions
test2 =
  let
    d = (1,0)
    n = (0,1)
    s = Sprite.player
    p = createPlayer d d n s
  in
    TestLabel "Directions Test #1" .
      TestCase $ assertEqual "" ((d, n)) (directions p)
-- Create AI
test3 =
  let
    p = (10, 5)
    d = (0, -1)
    l = [(1,1)]
    s = Sprite.ai
  in
    TestLabel "Create AI Test #1" .
      TestCase $ assertEqual "" (Computer p d l s) (createAI p d l s)
-- Paths
test4 =
  let
    l = [(0,0), (1,1), (2,2)]
    c = createAI (0,0) (0,0) l []
  in
    TestLabel "Paths Test #1" .
      TestCase $ assertEqual "" (l) (paths c)
-- Position
test5 =
  let
    p = (10, 20)
    d = (1, 0)
    c = createAI p d [] []
  in
    TestLabel "Position Test #1" .
      TestCase $ assertEqual "" (p) (position c)
-- Direction
test6 =
  let
    p = (10, 20)
    d = (1, 0)
    c = createAI p d [] Sprite.ai
  in
    TestLabel "Direction Test #1" .
      TestCase $ assertEqual "" (d) (direction c)
-- isAI
test7 =
  let
    c = createAI (10, 20) (1, 0) [] []
  in
    TestLabel "isAI Test #1" .
      TestCase $ assertEqual "" (True) (isAI c)
-- isAI
test8 =
  let
    p = createPlayer (0, 0) (1, 0) (0,0) []
  in
    TestLabel "isAI Test #1" .
      TestCase $ assertEqual "" (False) (isAI p)
-- makeMove
test9 =
  let
    p = createPlayer (10, 5) (1, 0) (0, 0) []
    s = (2.4, 2.4)
  in
    TestLabel "Make Move Test #1" .
      TestCase $ assertEqual "" ((12.4, 5.0)) (position (makeMove s p))
test10 =
  let
    p = createPlayer (-10, 5) (0, -1) (0, 0) []
    s = (2.4, 2.4)
  in
    TestLabel "Make Move Test #1" .
      TestCase $ assertEqual "" ((-10, 2.6)) (position (makeMove s p))
-- sprites
test11 =
  let
    p = createPlayer (0, 0) (1, 0) (0,0) Sprite.player
  in
    TestLabel "Sprites Test #1" .
      TestCase $ assertEqual "" (Sprite.player) (sprites p)
-- getPicture
test12 =
  let
    p = createPlayer (0, 0) (1, 0) (0, 0) Sprite.player
    (Sprite.Sprite x _) = (Sprite.player !! 1)
  in
    TestLabel "getPicture Test #1" .
      TestCase $ assertEqual "" (x) (getPicture p)
test13 =
  let
    p = createPlayer (0, 0) (-1, 0) (0, 0) Sprite.player
    (Sprite.Sprite t _) = head Sprite.player
  in
    TestLabel "getPicture Test #2" .
      TestCase $ assertEqual "" (t) (getPicture p)
-- pictureFromDirection
test14 =
  let
    s = Sprite.player
    (Sprite.Sprite t _) = head Sprite.player
  in
    TestLabel "pictureFromDirection Test #1" .
      TestCase $ assertEqual "" (t) (pictureFromDirection s (-1,0))
