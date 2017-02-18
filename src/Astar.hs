import Data.List
import qualified Test.HUnit as T
import Tile

{-
type Board = [Tile]
type Position = (Float, Float)
type Path = [Position]
type PossiblePath = (Int, Path)-}

{-
 F W W F F
 F W W F W
 F F F F W
 W F W F F
 F F W W W
-}



board :: Board
board = [Floor (-2,-2), Wall (-1,-2), Wall (0,-2), Floor (1,-2), Floor (2,-2),
         Floor (-2,-1), Wall (-1,-1), Wall (0,-1), Floor (1,-1), Wall (2,-1),
         Floor (-2,0), Floor (-1,0), Floor (0,0), Floor (1,0), Wall (2,0),
         Wall (-2,1), Floor (-1,1), Wall (0,1), Floor (1,1), Floor (2,1),
         Floor (-2,2), Floor (-1,2), Wall (0,2), Wall (1,2), Wall (2,2) ]

findNeighbour (x, y) =
  [ (a, b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], satisfy (x, y) (a,b) ]

satisfy :: Position -> Position -> Bool
satisfy (a, b) (c, d) =
  c > (-3) && c < 3 && d > (-3) && d < 3 && not ((c==a && d == b)) &&(c-a == 0 || d-b ==0)


newPaths :: Position -> Board -> [(Float, Float)]
newPaths current p =
  let
    n  = findNeighbour current
    n' = filter (isValidMove p) n
  in
    n'

distance :: Position -> Position -> Float
distance (x0, y0) (x1, y1) = abs (x1-x0) + abs (y1-y0)

cost :: Position -> Path -> Float
cost goal path = let
                   current = last path
                   steps   = fromIntegral (length path -1) :: Float
                 in
                   steps + (distance current goal)

{- pathLists board path
   PRE:
   POST: list of all valid neighbours of the last element in path appended to path 
-}
pathLists ::  Board -> Path -> [Path]
pathLists board path =
  let
    paths    = filter (`notElem` path) $ newPaths (last path) board
    pathList = map (\p->path++[p]) paths
  in
    pathList

{- isValidMove board pos
   PRE:
   POST: True if and only if 
   SIDE EFFECTS:
   VARIANT:
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

aStar :: Board -> Position -> Position -> [Path]
aStar board goal start = aStarAux board goal [[start]]
  
aStarAux :: Board -> Position -> [Path] -> [Path]
aStarAux board goal paths
  | any (\p -> last p == goal) paths = filter (\p-> last p == goal) paths
  | otherwise =
    let
      best = snd $ minimum $ costOfPaths paths goal
      next = pathLists board best
    in
      aStarAux board goal $ filter (/= best) paths ++ next
      

test1 = let
          goal = (2,1)
          start = (-2,-2)
        in
          T.TestCase $ T.assertEqual "aStar"
          ([[(-2.0,-2.0),(-2.0,-1.0),(-2.0,0.0),(-1.0,0.0),(0.0,0.0),(1.0,0.0),(1.0,1.0),(2.0,1.0)]])
          (aStar board goal start)

test2 = let
          goal = (-2,2)
          start = (2,-2)
        in
          T.TestCase $ T.assertEqual "aStar"
          ([[(2.0,-2.0),(1.0,-2.0),(1.0,-1.0),(1.0,0.0),(0.0,0.0),(-1.0,0.0),(-1.0,1.0),(-1.0,2.0),(-2.0,2.0)]])
          (aStar board goal start)











    
