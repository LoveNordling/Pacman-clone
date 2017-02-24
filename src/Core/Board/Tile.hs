module Core.Board.Tile (Tile(..), position, setPosition, hasTreasure, testSuite) where
import Test.HUnit
{-
  REPRESENTATION CONVENTION:
    A tile is an unmovable piece on the board.

    A floor is a valid path for a computer or the player and is given by Floor (a, b) t where (a, b) are the coordinates of the floor and t signifies if there is a treasure on that floor.

    A wall is an invalid path given by Wall (a, b), where (a,b) are the coordinates of the wall.

  REPRESENTATION INVARIANT:
    Tiles cannot have overlapping positions.
-}
data Tile = Floor (Int, Int) Treasure | Wall (Int, Int) deriving (Show, Eq)

type Treasure = Bool

{- position a
   PRE:       True
   POST:      The current position of a.
   EXAMPLES:  position (Floor (1, 1) True) == (1, 1)
-}
position :: Tile -> (Int, Int)
position (Floor a _) = a
position (Wall a)  = a

{- setPosition t x y
   PRE:       True
   POST:      t with new position (x, y)
   EXAMPLES:  setPosition (Floor (0,0) False) 5 5 == Floor (5, 5) False
-}
setPosition :: Tile -> Int -> Int -> Tile
setPosition (Floor _ i) x y = Floor (x, y) i
setPosition (Wall _)    x y = Wall (x, y)

{- hasTreasure t
   PRE:       True.
   POST:      True f t has a treasure, otherwise False.
   EXAMPLES:  hasTreasure (Floor (1,1) True) == True
              hasTreasure (Wall (1,1))       == False
-}
hasTreasure :: Tile -> Bool
hasTreasure (Floor _ i) = i
hasTreasure _           = False

-----------------------------------------
-- TEST CASES
-----------------------------------------
test1, test2, test3, test4, test5, test6, test7 :: Test
testSuite = TestList [test1, test2, test3, test4, test5, test6, test7]

-- position
test1 = TestLabel "position" $ TestCase $ assertEqual "Position" ((1,2)) (position (Floor (1,2) True))
test2 = TestLabel "position" $ TestCase $ assertEqual "Position" ((0,0)) (position (Floor (0,0) False))

-- setPosition
test3 = TestLabel "setPosition" $ TestCase $ assertEqual "setPosition" (Floor (3,4) True) (setPosition (Floor (1,2) True) 3 4)
test4 = TestLabel "setPosition" $ TestCase $ assertEqual "setPosition" (Wall (3,4)) (setPosition (Wall (1,2)) 3 4)

--hasTreasure
test5 = TestLabel "hasTreasure" $ TestCase $ assertEqual "hasTreasure" (True) (hasTreasure (Floor (1,2) True))
test6 = TestLabel "hasTreasure" $ TestCase $ assertEqual "hasTreasure" (False) (hasTreasure (Wall (1,2)))
test7 = TestLabel "hasTreasure" $ TestCase $ assertEqual "hasTreasure" (False) (hasTreasure (Floor (1,2) False))
