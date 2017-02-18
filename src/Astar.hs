import Data.List
import qualified Test.HUnit as T
import Core.Board.Tile

type Position = (Float, Float)
type Path = [Position]
type Board = [Tile]

{-
 F W W F F
 F W W F W
 F F F F W
 W F W F F
 F F W W W
-}


board :: Board
board = [Floor (-2,2), Wall (-1,2), Wall (0,2), Floor (1,2), Floor (2,2),
         Floor (-2,1), Wall (-1,1), Wall (0,1), Floor (1,1), Wall (2,1),
         Floor (-2,0), Floor (-1,0), Floor (0,0), Floor (1,0), Wall (2,0),
         Wall (-2,-1), Floor (-1,-1), Wall (0,-1), Floor (1,-1), Floor (2,-1),
         Floor (-2,-2), Floor (-1,-2), Wall (0,-2), Wall (1,-2), Wall (2,-2) ]

{- validCoordinates (x, y)
   PRE: True
   POST: all coordinates (x0, y0) that are neighbours (top, down, bottom, left) to (x, y) and on the board.
   SIDE EFFECTS: none
   EXAMPLES: findNeighbour (0,0) = [(0,1),(-1,0),(0,-1),(1,0)]
-}
validCoordinates :: Position -> [Position]
validCoordinates (x, y) =
  [ (a, b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], satisfy (x, y) (a, b) ]

{- satisfy (a,b) (c,d)
   PRE: True
   POST: True if and only if -3 < (c, d) < 3 and either c==a OR d==b.
   SIDE EFFECTS: none
   EXAMPLES:
-}
satisfy :: Position -> Position -> Bool
satisfy (a, b) (c, d) =
  --c > (-3) && c < 3 && d > (-3) && d < 3 &&
  not ((c == a && d == b)) &&(c-a == 0 || d-b == 0)

{- adjFloors current board
   PRE:
   POST: all neigbouring floors to current under board
   SIDE EFFECTS: none
   EXAMPLES:
-}
adjFloors :: Position -> Board -> [(Float, Float)]
adjFloors current board = filter (isValidMove board) $ validCoordinates current

{- distance pos1 pos2
   PRE:
   POST: the manhattan distance between pos1 and pos2
   SIDE EFFECTS: none
   EXAMPLES:
-}
distance :: Position -> Position -> Float
distance (x0, y0) (x1, y1) = abs (x1-x0) + abs (y1-y0)

{- cost goal path
   PRE:
   POST: the total estimated cost for path to reach goal
   SIDE EFFECTS: none
   EXAMPLES.
-}
cost :: Position -> Path -> Float
cost goal path = let
                   current = last path
                   steps   = fromIntegral (length path -1) :: Float
                 in
                   steps + (distance current goal)

{- newPaths board path
   PRE:
   POST: list of all valid neighbours of the last element in path each appended to path
-}
newPaths ::  Board -> Path -> [Path]
newPaths board path =
  let
    paths    = filter (`notElem` path) $ adjFloors (last path) board
    pathList = map (\p->path++[p]) paths
  in
    pathList

{- isValidMove board pos
   PRE:
   POST: True if and only if pos is on board and is not a wall
   SIDE EFFECTS: none
   VARIANT: length of board
   EXAMPLES:
-}
isValidMove :: Board -> Position -> Bool
isValidMove [] _       = False
isValidMove ((Wall (x,y)):bs) (x0, y0)
  | x == x0 && y == y0 = False
  | otherwise          = isValidMove bs (x0,y0)
isValidMove ((Floor (x,y)):bs) (x0,y0)
  | x == x0 && y == y0 = True
  | otherwise          = isValidMove bs (x0, y0)

{- costOfPaths paths goal
   PRE:
   POST: list of every path in paths mapped to the projected cost of the last tile in it
-}
costOfPaths :: [Path] -> Position -> [(Float, Path)]
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
aStar :: Board -> Position -> Position -> Path
aStar board goal start = aStarAux board goal [[start]] where

        {- aStarAux board goal paths
        PRE:
        POST:
        SIDE EFFECTS:
        VARIANT:
        EXAMPLES:
        -}
        aStarAux :: Board -> Position -> [Path] -> Path
        aStarAux board _ [] = []
        aStarAux board goal paths
          | any (\p -> last p == goal) paths = head (filter (\p-> last p == goal) paths)
          | otherwise =
            let
              best = snd . minimum $ costOfPaths paths goal
              next = newPaths board best
            in
              aStarAux board goal $ filter (/= best) paths ++ next

{-
 F W W F F
 F W W F W
 F F F F W
 W F W F F
 F F W W W
-}


test1 = let
          goal = (2,2)
          start = (-2,-2)
        in
          T.TestCase $ T.assertEqual "aStar"
          ([(-2.0,-2.0),(-1.0,-2.0),(-1.0,-1.0),(-1.0,0.0),(0.0,0.0),(1.0,0.0),(1.0,1.0),(1.0,2.0),(2.0,2.0)])
          (aStar board goal start)

test2 = let
          goal = (1,3)
          start = (3,8)
          tiles = [
               Wall (0,0), Wall (1,0), Wall (2,0), Wall (3,0), Wall (4,0), Wall (5,0), Wall (6,0), Wall (7,0), Wall (8,0), Wall (9,0),
               Wall (0,1), Floor (1,1), Floor (2,1), Floor (3,1), Wall (4,1), Wall (5,1), Floor (6,1), Wall (7,1), Floor (8,1), Wall (9,1),
               Wall (0,2), Floor (1,2), Wall (2,2), Floor (3,2), Floor (4,2), Wall (5,2), Floor (6,2), Floor (7,2), Floor (8,2), Wall (9,2),
               Wall (0,3), Floor (1,3), Wall (2,3), Wall (3,3), Floor (4,3), Floor (5,3), Floor (6,3), Wall (7,3), Floor (8,3), Wall (9,3),
               Wall (0,4), Floor (1,4), Wall (2,4), Floor (3,4), Floor (4,4), Wall (5,4), Floor (6,4), Floor (7,4), Floor (8,4), Wall (9,4),
               Wall (0,5), Floor (1,5), Floor (2,5), Floor (3,5), Wall (4,5), Wall (5,5), Floor (6,5), Wall (7,5), Floor (8,5), Wall (9,5),
               Wall (0,6), Floor (1,6), Wall (2,6), Floor (3,6), Floor (4,6), Wall (5,6), Floor (6,6), Wall (7,6), Floor (8,6), Wall (9,6),
               Wall (0,7), Floor (1,7), Wall (2,7), Wall (3,7), Floor (4,7), Floor (5,7), Floor (6,7), Wall (7,7), Floor (8,7), Wall (9,7),
               Wall (0,8), Floor (1,8), Floor (2,8), Floor (3,8), Floor (4,8), Wall (5,8), Floor (6,8), Floor (7,8), Floor (8,8), Wall (9,8),
               Wall (0,9), Wall (1,9), Wall (2,9), Wall (3,9), Wall (4,9), Wall (5,9), Wall (6,9), Wall (7,9), Wall (8,9), Wall (9,9)
             ]
        in
          T.TestCase $ T.assertEqual "aStar"
          ([(3.0,8.0),(2.0,8.0),(1.0,8.0),(1.0,7.0),(1.0,6.0),(1.0,5.0),(1.0,4.0),(1.0,3.0)])
          (aStar tiles goal start)
