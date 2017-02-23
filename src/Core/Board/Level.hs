module Core.Board.Level (Level, setLevel, getBoard, checkForTreasure, spawnPosition) where

import Data.Array
import qualified Core.Board.Board as Board
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Tile as Tile

-- REPRESENTATION:

-- SSecond is AI position, third level number, fourth level goal
data Level = Level Board.Board (Float, Float) Int Int deriving (Show)

{- setLevel i
   PRE:           i must exist=
   EXAMPLES:      setLevel ==
-}
setLevel :: Int -> Maybe (Level, (Float, Float))
setLevel i
  | (length levels - 1) < i = Nothing
  | otherwise =
    let
      tiles = levels !! i
      coord = coords !! i
      board = Board.createBoard tiles
      count = countTreasures tiles
      level = Level board (snd coord) i count
    in
      Just (level, (fst coord))
    where
      {- countTreasures b
         PRE:
         POST:
         EXAMPLES:  countTreasures ==
         VARIANT:   |b|
      -}
      countTreasures :: [[Tile.Tile]] -> Int
      countTreasures tiles = sum (map (foldl sumTreasures 0) tiles)
      {- findTreasures b
         PRE:       ...
         POST:      ...
         EXAMPLES:  findTreasures ==
         VARIANT:   |b|
      -}
      sumTreasures :: Int -> Tile.Tile -> Int
      sumTreasures i t
        | Tile.hasTreasure t = i + 1
        | otherwise = i

{- getBoard b l s
   PRE:           True
   POST:          b
   EXAMPLES:      getBoard ==
-}
getBoard :: Level -> Board.Board
getBoard (Level b _ _ _) = b

{- checkForTreasure l s p
   PRE:       p must be valid coordinates for the level.
   POST:      If p has treasure, then l without a treasure on that position and s + 1, otherwise l and s as they are.
   EXAMPLES:  checkForTreasure ==
-}
checkForTreasure :: Level -> (Int, Int) -> Maybe Level
checkForTreasure level@(Level board cp n g) p =
  if foundTreasure board p
    then Just (Level (board // [(p, (Tile.Floor p False))]) cp n g)
    else Nothing
    where
      {- foundTreasure arguments
         PRE:           pre-condition on the arguments
         POST:          post-condition on the result, in terms of the arguments
         SIDE EFFECTS:  if any, including exceptions
         EXAMPLES:      foundTreasure ==
         VARIANT:       None
      -}
      foundTreasure :: Board.Board -> (Int, Int) -> Bool
      foundTreasure board position = Tile.hasTreasure (board ! position)

spawnPosition :: Level -> (Float, Float)
spawnPosition (Level _ p _ _) = p

-- LEVELS

itemFloor = Tile.Floor (0,0) True
baseFloor = Tile.Floor (0, 0) False
baseWall  = Tile.Wall (0, 0)

u = itemFloor
x = baseWall
o = baseFloor

-- first element of the tuple is player start coord, second is AI start coord
coords = [ ((1.0, 1.0), (8.0, 8.0)), ((1.0, 1.0), (10.0, 11.0)) ]
levels = [ hardcodedMap1, level1 ]

level1 = [
  [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x],
  [x,u,u,u,u,u,u,u,u,x,u,u,u,u,u,u,u,u,x],
  [x,u,x,x,u,x,x,x,u,x,u,x,x,x,u,x,x,u,x],
  [x,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,x],
  [x,u,x,x,u,x,u,x,x,x,x,x,u,x,u,x,x,u,x],
  [x,u,u,u,u,x,u,u,u,x,u,u,u,x,u,u,u,u,x],
  [x,x,x,x,u,x,x,x,u,x,u,x,x,x,u,x,x,x,x],
  [x,x,x,x,u,x,o,o,o,o,o,o,o,x,u,x,x,x,x],
  [x,x,x,x,u,x,o,x,x,o,x,x,o,x,u,x,x,x,x],
  [x,x,x,x,u,o,o,x,o,o,o,x,o,o,u,x,x,x,x],
  [x,x,x,x,u,x,o,x,x,x,x,x,o,x,u,x,x,x,x],
  [x,x,x,x,u,x,o,o,o,o,o,o,o,x,u,x,x,x,x],
  [x,x,x,x,u,x,o,x,x,x,x,x,o,x,u,x,x,x,x],
  [x,u,u,u,u,u,u,u,u,x,u,u,u,u,u,u,u,u,x],
  [x,u,x,x,u,x,x,x,u,x,u,x,x,x,u,x,x,u,x],
  [x,u,u,x,u,u,u,u,u,u,u,u,u,u,u,x,u,u,x],
  [x,x,u,x,u,x,u,x,x,x,x,x,u,x,u,x,u,x,x],
  [x,u,u,u,u,x,u,u,u,x,u,u,u,x,u,u,u,u,x],
  [x,u,x,x,x,x,x,x,u,x,u,x,x,x,x,x,x,u,x],
  [x,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,u,x],
  [x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x]
 ]

hardcodedMap1 = [
   [baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall],
   [baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseWall, baseFloor, baseWall, baseFloor, baseWall],
   [baseWall, baseFloor, baseWall, baseFloor, baseFloor, baseWall, baseFloor, itemFloor, baseFloor, baseWall],
   [baseWall, baseFloor, baseWall, baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseFloor, baseWall],
   [baseWall, baseFloor, baseWall, baseFloor, baseFloor, baseWall, baseFloor, baseFloor, baseFloor, baseWall],
   [baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseWall, baseFloor, baseWall, baseFloor, baseWall],
   [baseWall, baseFloor, baseWall, baseFloor, baseFloor, baseWall, baseFloor, baseWall, baseFloor, baseWall],
   [baseWall, baseFloor, baseWall, baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseFloor, baseWall],
   [baseWall, baseFloor, itemFloor, baseFloor, baseFloor, baseWall, baseFloor, itemFloor, baseFloor, baseWall],
   [baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall]
   ]
