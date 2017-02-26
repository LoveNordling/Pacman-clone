module Core.Board.Tile
        ( Tile(..), position, setPosition, hasTreasure, testSuite ) where

-- Modules for testing
import Test.HUnit

{-# ANN module "HLint: Ignore Use ." #-}

{-
  REPRESENTATION CONVENTION:
    A tile is an unmovable piece on the board. A floor is a valid path for a computer or the player and is given by Floor (a, b) t where (a, b) are the coordinates of the floor and t signifies if there is a treasure on that tile. A wall is an invalid path given by Wall (a, b), where (a,b) are the coordinates of the wall.

  REPRESENTATION INVARIANT:
    None.
-}
data Tile = Floor (Int, Int) Treasure | Wall (Int, Int)
          deriving (Show, Eq) -- For unit testing

-- Represents a treasure on a tile
type Treasure = Bool

{- position a
   PRE:           True
   POST:          The position of a
   SIDE EFFECTS:  None
   EXAMPLES:      position (Floor (1, 1) True) == (1, 1)
-}
position :: Tile -> (Int, Int)
position (Floor a _) = a
position (Wall a)  = a

{- setPosition t x y
   PRE:           True
   POST:          t with new position (x, y)
   SIDE EFFECTS:  None
   EXAMPLES:      setPosition (Floor (0,0) False) 5 5 == Floor (5, 5) False
-}
setPosition :: Tile -> Int -> Int -> Tile
setPosition (Floor _ i) x y = Floor (x, y) i
setPosition (Wall _)    x y = Wall (x, y)

{- hasTreasure t
   PRE:           True
   POST:          True if t has a treasure, otherwise False
   SIDE EFFECTS:  None
   EXAMPLES:      hasTreasure (Floor (1,1) True) == True
                  hasTreasure (Wall (1,1))       == False
-}
hasTreasure :: Tile -> Bool
hasTreasure (Floor _ i) = i
hasTreasure _           = False

-----------------------------------------
-- TEST CASES
-----------------------------------------
test1, test2, test3, test4, test5, test6 :: Test
testSuite = TestList [ test1, test2, test3, test4, test5, test6 ]

-- position
test1 = TestLabel "Position Test #1" $ TestCase $ assertEqual "Should return the position" ((1,2)) (position (Floor (1,2) True))
test2 = TestLabel "Position Test #2" $ TestCase $ assertEqual "Should return the position" ((0,0)) (position (Floor (0,0) False))
-- setPosition
test3 = TestLabel "Set Position Test #1" $ TestCase $ assertEqual "Should return a tile with new position" ((3,4)) (position $ setPosition (Floor (1,2) True) 3 4)
-- hasTreasure
test4 = TestLabel "Has Treasure Test #1" $ TestCase $ assertEqual "Should return True" (True) (hasTreasure (Floor (1,2) True))
test5 = TestLabel "Has Treasure Test #2" $ TestCase $ assertEqual "Should return False" (False) (hasTreasure (Wall (1,2)))
test6 = TestLabel "Has Treasure Test #3" $ TestCase $ assertEqual "Should return False" (False) (hasTreasure (Floor (1,2) False))
