import Tile
import Debug.Trace
import Data.Maybe


type ValidPathsFunc = (Grid -> Position -> Path)
type CostFunc       = (Position -> Path -> Float)
type Grid = [[Tile]]
type Path = [Position]
{-
  F F W F
  F W W F
  F F F F
  W W W W-}
testBoard :: Grid
testBoard = [[Tile (0.0,0.0) Floor Void, Tile (0.0,1.0) Floor Void, Tile (0.0,2.0) Wall Void, Tile (0.0,3.0) Floor Void],
             [Tile (1.0,0.0) Floor Void, Tile (1.0,1.0) Wall Void, Tile (1.0,2.0) Wall Void,  Tile (1.0,3.0) Floor Void],
             [Tile (2.0,0.0) Floor Void, Tile (2.0,1.0) Floor Void, Tile (2.0,2.0) Floor Void, Tile (2.0,3.0) Floor Void],
             [Tile (3.0,0.0) Wall Void, Tile (3.0,1.0) Wall Void, Tile (3.0,2.0) Wall Void, Tile (3.0,3.0) Wall Void]]


{- aStar grid goal pos vpf cf
   PRE: grid contains a Tile with coordinates goal and coordinates pos
   POST: 
   SIDE EFFECTS: none
   EXAMPLES:
-}
aStar :: Grid -> Position -> Position -> ValidPathsFunc -> CostFunc -> Path
aStar grid goal pos vpf cf = head $ aStarAux grid goal vpf cf [[pos]]

aStarAux :: Grid -> Position -> ValidPathsFunc -> CostFunc -> [Path] -> [Path]
aStarAux grid goal vpf cf paths
  | any (\p -> last p == goal) paths = filter (\p -> last p == goal) paths
  | otherwise                        = let
                                         best = snd $ minimum $ zip (map (cf goal) paths) paths
                                         pb = addRoutes grid paths best vpf
                                       in
                                         aStarAux grid goal vpf cf $ filter (/= best) paths ++ pb

addRoutes :: Grid -> [Path] -> Path -> ValidPathsFunc -> [Path]
addRoutes grid paths path vpf = let
                               cpaths = concat paths
                             in
                               [ path ++ [p] | p <- filter (`notElem` cpaths) $ vpf grid $ last path]

    
cost :: Position -> Path -> Float
cost goal path = let
                   current = last path
                   tCost = fromIntegral (length path -1) :: Float
                 in
                   tCost + (distance current goal)

distance :: Position -> Position -> Float
distance (x0, y0) (x1, y1) = abs (x1-x0) + abs (y1-y0)


validPaths :: Grid -> Position -> Path
validPaths grid (x, y) = [(x1, y1) | x1 <- [(x-1)..(x+1)],
                                     y1 <- [(y-1)..(y+1)],
                                     x1 >= 0,
                                     y1 >= 0,
                                     x1 <  (fromIntegral (length grid) :: Float),
                                     y1 <  (fromIntegral (length grid) :: Float),
                                     isNotWall (grid !! round x1 !! round y1),
                                     x-x1 ==0 || y-y1==0
                                     ]

isNotWall :: Tile -> Bool
isNotWall (Tile _ Floor _) = True
isNotWall (Tile _ Wall _)  = False
