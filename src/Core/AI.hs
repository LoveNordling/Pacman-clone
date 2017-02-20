module Core.AI (aStar) where
import Data.List
import Data.Array
import Core.Board.Tile
import Core.Board.Board
import Debug.Trace
{- validCoordinates arguments
   PRE:       True
   POST:      All coordinates that are adjacent to p.
   EXAMPLES:  validCoordinates  ==
-}
validCoordinates :: (Int, Int) -> [(Int, Int)]
validCoordinates (x, y) =
  [ (a, b) | a <- [(x-1)..(x+1)], b <- [(y-1)..(y+1)], satisifies (x, y) (a, b) ]
  where
    {- satisifies (a,b) (c,d)
       PRE:       True
       POST:      True if and only if c-a == 0 or d-b == 0, while also c =0 a and d == b, otherwise False.
       EXAMPLES:  ...
    -}
    satisifies :: (Int, Int) -> (Int, Int) -> Bool
    satisifies (a, b) (c, d) =
      not ((c == a && d == b)) &&(c-a == 0 || d-b == 0)

adjacentFloors :: (Int, Int) -> Board -> [(Int, Int)]
adjacentFloors position board = filter (isValidMove board) $ validCoordinates position
  where
    {- isValidMove board pos
       PRE:
       POST: True if and only if pos is on board and is not a wall
       SIDE EFFECTS: none
       VARIANT: length of board
       EXAMPLES:
    -}
    isValidMove :: Board -> (Int, Int) -> Bool
    isValidMove b p = case (board ! p) of
                        (Wall _) -> False
                        _        -> True

{- distance pos1 pos2
   PRE:       ...
   POST:      the manhattan distance between pos1 and pos2
   EXAMPLES:  ...
-}
distance :: (Int, Int) -> (Int, Int) -> Int
distance (x0, y0) (x1, y1) = abs (x1-x0) + abs (y1-y0)

{- cost goal path
   PRE:
   POST: the total estimated cost for path to reach goal
   SIDE EFFECTS: none
   EXAMPLES.
-}
cost :: (Int, Int) -> [(Int, Int)] -> Int
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
