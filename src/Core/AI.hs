module Core.AI
        ( aStar, testSuite ) where

-- Modules for testing
import Test.HUnit

-- External modules
import Data.List
import Data.Array

-- Internal modules
import qualified Core.Board.Tile as Tile hiding (testSuite)
import qualified Core.Board.Board as Board hiding (testSuite)
import qualified Core.Board.Level as Level hiding (testSuite, testSuiteExceptions)

{-# ANN module "HLint: Ignore Use fmap" #-}

{- validCoordinates p
   PRE:           True
   POST:          All coordinates that are adjacent to p
   SIDE EFFECTS:  None
   EXAMPLES:      validCoordinates (1,10) == [(0,10),(1,9),(1,11),(2,10)]
                  validCoordinates (0,-5) == [(-1,-5),(0,-6),(0,-4),(1,-5)]
-}
validCoordinates :: (Int, Int) -> [(Int, Int)]
validCoordinates (x, y) = [ (a, b) | a <- [(x-1)..(x+1)]
                                   , b <- [(y-1)..(y+1)], satisfies (x, y) (a, b) ]
  where
    {- satisifies (a,b) (c,d)
       PRE:           True
       POST:          True if and only if either a==c or b==d but not both, otherwise False
       SIDE EFFECTS:  None
       EXAMPLES:      satisfies (1,2) (3,4) = False
                      satisfies (1,2) (1,3) = True
                      satisfies (1,2) (1,2) = False
    -}
    satisfies :: (Int, Int) -> (Int, Int) -> Bool
    satisfies (a, b) (c, d) =
      not ((c == a && d == b)) && (c-a == 0 || d-b == 0)

{- adjacentFloors p b
   PRE:           p must be valid coordinates in b
   POST:          Positions of all adjacent coordinates that are valid in b
   SIDE EFFECTS:  Prints error message to the screen if p is not valid coordinates in b
   EXAMPLES:      Calling adjacentFloors on (1,1) and a board with the only valid adjacent tiles (1, 2) and (2, 1) would give a list with elements (1, 2) and (2, 1)
-}
adjacentFloors :: (Int, Int) -> Board.Board -> [(Int, Int)]
adjacentFloors position board = filter (isValidMove board) $ validCoordinates position
  where
    {- isValidMove b p
       PRE:           p are valid coordinates on b
       POST:          False if p is a wall, otherwise True
       SIDE EFFECTS:  Prints error message to the screen if p is not valid coordinates in b
       VARIANT:       |b|
       EXAMPLES:      Calling isValidMove on (1,1) and a board with a wall at position (1, 1) gives False
    -}
    isValidMove :: Board.Board -> (Int, Int) -> Bool
    isValidMove b p = case (board ! p) of
                        (Tile.Wall _) -> False
                        _             -> True

{- distance pos1 pos2
   PRE:           True
   POST:          The manhattan distance between pos1 and pos2
   SIDE EFFECTS:  None
   EXAMPLES:      distance (1,2) (2,3)  == 2
                  distance (-1,4) (0,0) == 5
-}
distance :: (Int, Int) -> (Int, Int) -> Int
distance (x0, y0) (x1, y1) = abs (x1-x0) + abs (y1-y0)

{- cost goal path
   PRE:           path is not empty
   POST:          The total estimated cost from path to goal
   SIDE EFFECTS:  None
   EXAMPLES:      cost (1,1) [] == 0
-}
cost :: (Int, Int) -> [(Int, Int)] -> Int
cost goal []   = 0
cost goal path = let current = last path
                     steps   = length path - 1
                 in  steps + (distance current goal)

{- newPaths b p
   PRE:           Position in p must be valid coordinates in b
   POST:          All valid neighbours in b of the last element in p, each appended to p
   SIDE EFFECTS:  Prints an error message if position in p is not a valid coordinate in b
   EXAMPLES:      Calling newPaths on a 10x10 board and (1,1), where the element at (1,1) in the board has the valid neighbours (1,2) and (2,1) would give a list with elements [(1,1), (1,2)] and [(1,1), (2,1)]
-}
newPaths :: Board.Board -> [(Int, Int)] -> [[(Int, Int)]]
newPaths board path =
   let
     paths = filter (`notElem` path) $ adjacentFloors (last path) board
     pathList = map (\p->path++[p]) paths
   in
     pathList

{- costOfPaths p g
   PRE:           True
   POST:          Every path in p mapped to the projected cost to g
   SIDE EFFECTS:  None
   EXAMPLES:      costOfPaths [] (1,1) == []
                  costOfPaths [[(1,1), (2,1), (3,1), (3,2)]] (3,3) == [(4, [(1,1), (2,1), (3,1), (3,2)])]
                  costOfPaths [[(1,1), (2,1)], [(1,1), (1,2), (1,3)] ] (3,1) == [(2, [(1,1), (2,1)]), (6, [(1,1), (1,2), (1,3)])]
-}
costOfPaths :: [[(Int, Int)]] -> (Int, Int) -> [(Int, [(Int, Int)])]
costOfPaths paths goal = let costs = map (cost goal) paths
                         in  zip costs paths

{- aStar b g s
   PRE:           g and s are valid coordinates in b
   POST:          The shortest path from s to g in b
   SIDE EFFECTS:  Prints an error message if either g or p are  invalid coordiantes in b
   EXAMPLES:      aStar b (6,6) (8,8), where b is a board where the shortest path from (6,6) to (8,8) is through coordinates (6, 7) and (7, 7), gives the list [(8,7), (7,7), (6,7), (6,6)]
-}
aStar :: Board.Board -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
aStar board goal start = aStarAux board goal [[start]] where
        {- aStarAux b g p
           PRE:           g and p are valid coordinates in b
           POST:          The shortest path from s to g in b
           SIDE EFFECTS:  Prints an error message if either g or the positions in p are invalid coordiantes in b
           EXAMPLES:      aStarAux b (6,6) [[(8,8)]], where b is a board where the shortest path from (6,6) to (8,8) is through coordinates (6, 7) and (7, 7), gives the list [(8,7), (7,7), (6,7), (6,6)]
           VARIANT:       Number of floors in b
        -}
        aStarAux :: Board.Board -> (Int, Int) -> [[(Int, Int)]] -> [(Int, Int)]
        aStarAux board _ [] = []
        aStarAux board goal paths
          | any (\p -> last p == goal) paths = tail (head (filter (\p-> last p == goal) paths))
          | otherwise =
            let
              best = snd . minimum $ costOfPaths paths goal
              next = newPaths board best
            in
              aStarAux board goal $ filter (/= best) paths ++ next

-------------------------------------------
-- TEST SUITE
-------------------------------------------
test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12 :: Test
testSuite = TestList [ test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12]
-- Setups
testBoard = let (Just(l,x)) = Level.setLevel 0 in Level.getBoard l
-- validCoordinates
test1 = TestLabel "Valid Coordinates #1" $ [(-1,0), (0,-1),(0,1), (1,0)] ~=? (validCoordinates (0,0))
test2 = TestLabel "Valid Coordinates #2" $ [(-2,-1), (-1,-2),(-1,0), (0,-1)] ~=? (validCoordinates (-1,-1))
test3 = TestLabel "Valid Coordinates #3" $ [(0,2), (1,1),(1,3), (2,2)] ~=? (validCoordinates (1,2))
-- adjacentFloors
test4 =
    TestLabel "Adjacent floors Test #1" .
      TestCase $ assertEqual "Should return adjacent floors" [(4,1),(4,3),(5,2)] (adjacentFloors (4,2) testBoard)
test5 =
    TestLabel "Adjacent floors Test #2" .
      TestCase $ assertEqual "Should return adjacent floors" [(1,2),(2,1)] (adjacentFloors (1,1) testBoard)
-- distance
test6 = TestLabel "Distance Test #1" .
          TestCase $ assertEqual "Should return distance 7" (7) (distance (0,0) (3,4))
test7 = TestLabel "Distance Test #2" .
          TestCase $ assertEqual "Should return distance 0" (0) (distance (0,0) (0,0))
test8 = TestLabel "Distance Test #3" .
          TestCase $ assertEqual "Should return distance 2" (2) (distance (1,1) (0,0))
-- cost
test9 =
  let
    current = [(1,1),(1,2)]
    goal    = (1,3)
  in
    TestLabel "Cost Test #1" .
      TestCase $ assertEqual "" ((length current - 1) + distance (last current) goal) (cost goal current)
test10 =
  let
    current = [(1,1)]
    goal    = (1,1)
  in
    TestLabel "Cost Test #2" .
      TestCase $ assertEqual "cost" ((length current -1) +distance (last current) goal) (cost goal current)
test17 =
  let
    current = []
    goal = (1,1)
  in
    TestLabel "Cost Test #3" .
      TestCase $ assertEqual "cost" (-1) (cost goal current)
-- newpaths
test11 =
    TestLabel "New Paths test #1" .
      TestCase $ assertEqual "newPaths" ([[(1,1),(1,2)],[(1,1),(2,1)]]) (newPaths testBoard [(1,1)])

-- costOfPaths
test12 =
  let
    paths = newPaths testBoard [(1,1)]
    goal  = (4,2)
  in
    TestLabel "Cost of Paths Test #1" .
      TestCase $ assertEqual "newPaths" (distance (head $ head paths) goal) (fst.head $ costOfPaths paths goal)
-- aStar
test13 =
  let
    start = (1,1)
    goal = (4,3)
  in
    TestLabel "A-Star Test #1" .
      TestCase $ assertEqual "newPaths" ([(2,1),(3,1),(4,1),(4,2),(4,3)]) (aStar testBoard goal start)

-- satisfies
test14 = TestLabel "A-Star Test #2" .
          TestCase $ assertEqual "satisfies" (False) (satisfies (1,2) (2,3))
            where satisfies (a, b) (c, d) = not ((c == a && d == b)) &&(c-a == 0 || d-b == 0)

test15 = TestLabel "A-Star Test #2" .
          TestCase $ assertEqual "satisfies" (True) (satisfies (1,1) (1,3))
            where satisfies (a, b) (c, d) = not ((c == a && d == b)) &&(c-a == 0 || d-b == 0)

test16 = TestLabel "A-Star Test #3" .
          TestCase $ assertEqual "satisfies" (False) (satisfies (1,0) (0,1))
            where satisfies (a, b) (c, d) = not ((c == a && d == b)) &&(c-a == 0 || d-b == 0)
