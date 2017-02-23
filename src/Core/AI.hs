module Core.AI (aStar, testSuite) where

import Data.List
import Data.Array
import Test.HUnit

import Core.Board.Tile
import Core.Board.Board
import qualified Core.Board.Level as L

{- validCoordinates arguments
   PRE:       True
   POST:      All coordinates that are adjacent to p.
   EXAMPLES:  validCoordinates  ==
-}
validCoordinates :: (Int, Int) -> [(Int, Int)]
validCoordinates (x, y) =
  [ (a, b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], satisfies (x, y) (a, b) ]
  where
    {- satisifies (a,b) (c,d)
       PRE:       True
       POST:      True if and only if either a==c or b==d but not both, otherwise False.
       EXAMPLES:  satisfies (1,2) (3,4) = False
                  satisfies (1,2) (1,3) = True
                  satisfies (1,2) (1,2) = False
    -}
    satisfies :: (Int, Int) -> (Int, Int) -> Bool
    satisfies (a, b) (c, d) =
      not ((c == a && d == b)) &&(c-a == 0 || d-b == 0)

-- TODO: ADD SPECIFICATION
adjacentFloors :: (Int, Int) -> Board -> [(Int, Int)]
adjacentFloors position board = filter (isValidMove board) $ validCoordinates position
  where
    {- isValidMove board pos
       PRE:          pos are valid coordinates on board
       POST:         True if and only if pos is on board and is not a wall
       SIDE EFFECTS: none
       VARIANT:      length of board
       EXAMPLES:     isValidMove (Core.Board.Board.getBoard $ Core.Board.Board.setLevel 0) (1,1) = True
                     isValidMove (Core.Board.Board.getBoard $ Core.Board.Board.setLevel 0) (0,0) = False
    -}
    isValidMove :: Board -> (Int, Int) -> Bool
    isValidMove b p = case (board ! p) of
                        (Wall _) -> False
                        _        -> True

{- distance pos1 pos2
   PRE:          True
   POST:         the manhattan distance between pos1 and pos2
   SIDE EFFECTS: none
   EXAMPLES:     distance (1,2) (2,3)  = 2
                 distance (-1,4) (0,0) = 5
-}
distance :: (Int, Int) -> (Int, Int) -> Int
distance (x0, y0) (x1, y1) = abs (x1-x0) + abs (y1-y0)

{- cost goal path
   PRE: path is not empty
   POST: the total estimated cost for path to reach goal
   SIDE EFFECTS: none
   EXAMPLES.
-}
cost :: (Int, Int) -> [(Int, Int)] -> Int
cost goal []      = -1
cost goal path = let
                   current = last path
                   steps   = length path - 1
                 in
                   steps + (distance current goal)


{- newPaths board path
   PRE:     ...
   POST:    list of all valid neighbours of the last element in path each appended to path
   EXAMPLES: ...
-}
newPaths :: Board -> [(Int, Int)] -> [[(Int, Int)]]
newPaths board path =
   let
     paths = filter (`notElem` path) $ adjacentFloors (last path) board
     pathList = map (\p->path++[p]) paths
   in
     pathList

{- costOfPaths paths goal
  PRE:
  POST: list of every path in paths mapped to the projected cost of the last tile in it
-}
costOfPaths :: [[(Int, Int)]] -> (Int, Int) -> [(Int, [(Int, Int)])]
costOfPaths paths goal = let
                          costs = map (cost goal) paths
                        in
                          zip costs paths

{- aStar board goal start
   PRE: goal and start is on board
   POST: the shortest path from start to goal under board
   SIDE EFFECTS: none
   EXAMPLES:
-}
aStar :: Board -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
aStar board goal start = aStarAux board goal [[start]] where

        {- aStarAux board goal paths
        PRE:
        POST:
        SIDE EFFECTS:
        VARIANT:
        EXAMPLES:
        -}
        aStarAux :: Board -> (Int, Int) -> [[(Int, Int)]] -> [(Int, Int)]
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
-- validCoordinates
test1 = [(-1,0), (0,-1),(0,1), (1,0)] ~=? (validCoordinates (0,0))
test2 = [(-2,-1), (-1,-2),(-1,0), (0,-1)] ~=? (validCoordinates (-1,-1))
test3 = [(0,2), (1,1),(1,3), (2,2)] ~=? (validCoordinates (1,2))
-- adjacentFloors
test4 = let
          (Just(l,x)) = L.setLevel 0
          board = L.getBoard l
          current = (4,2)
        in
          TestLabel "Adjacent floors" $ TestCase $ assertEqual "adjacentFloors"
          [(4,1),(4,3),(5,2)] (adjacentFloors current board)

test5 = let
          (Just(l,x)) = L.setLevel 0
          board = L.getBoard l
          current = (1,1)
        in
          TestCase $ assertEqual "adjacentFloors"
          [(1,2),(2,1)] (adjacentFloors current board)

-- distance
test6 = TestCase $ assertEqual "distance" (7) (distance (0,0) (3,4))
test7 = TestCase $ assertEqual "distance" (0) (distance (0,0) (0,0))

test8 = TestCase $ assertEqual "distance" (2) (distance (1,1) (0,0))

-- cost
test9 = let
          current = [(1,1),(1,2)]
          goal    = (1,3)
        in
          TestCase $ assertEqual "cost"
          ((length current - 1) +distance (last current) goal) (cost goal current)

test10 = let
          current = [(1,1)]
          goal    = (1,1)
         in
          TestCase $ assertEqual "cost"
          ((length current -1) +distance (last current) goal) (cost goal current)

test17 = let
           current = []
           goal = (1,1)
         in
           TestCase $ assertEqual "cost"
           (-1) (cost goal current)
-- newpaths

test11 = let
          (Just(l,x)) = L.setLevel 0
          board = L.getBoard l
          current = (4,2)
         in
          TestCase $ assertEqual "newPaths"
          ([[(1,1),(1,2)],[(1,1),(2,1)]]) (newPaths board [(1,1)])

-- costOfPaths

test12 = let
          (Just(l,x)) = L.setLevel 0
          board = L.getBoard l
          paths = newPaths board [(1,1)]
          goal = (4,2)
         in
          TestCase $ assertEqual "newPaths"
          (distance (head $ head paths) goal) (fst.head $ costOfPaths paths goal)

-- aStar
test13 = let
          (Just(l,x)) = L.setLevel 0
          board = L.getBoard l
          start = (1,1)
          goal = (4,3)
         in
          TestCase $ assertEqual "newPaths"
          ([(2,1),(3,1),(4,1),(4,2),(4,3)]) (aStar board goal start)

-- satisfies
test14 =  TestCase $ assertEqual "satisfies"
          (False) (satisfies (1,2) (2,3)) where satisfies (a, b) (c, d) = not ((c == a && d == b)) &&(c-a == 0 || d-b == 0)

test15 =  TestCase $ assertEqual "satisfies"
          (True) (satisfies (1,1) (1,3)) where satisfies (a, b) (c, d) = not ((c == a && d == b)) &&(c-a == 0 || d-b == 0)

test16 =  TestCase $ assertEqual "satisfies"
          (False) (satisfies (1,0) (0,1)) where satisfies (a, b) (c, d) = not ((c == a && d == b)) &&(c-a == 0 || d-b == 0)
