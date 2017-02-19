module Core.Pathfinder (aStar) where
import Data.List
import Core.Board.Tile

type Position = (Float, Float)
type Path = [Position]
type Board = [Tile]

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
