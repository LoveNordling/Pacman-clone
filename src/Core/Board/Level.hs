module Core.Board.Level
        ( Level, setLevel, getBoard, getLevelGoal, checkForTreasure, spawnPosition, nextLevel )
where

import Data.Array

import qualified Core.Board.Board as Board
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Tile as Tile
import qualified Core.Extras.Resources as Resources

{-
  REPRESENTATION CONVENTION:
    A level is given by Level b xy n g. The first argument, b, is the board, map, of the level while the second is the default position for the AIs to spawn on. The level number is n, and g is the number of treasures on a map.

  REPRESENTATION INVARIANT:
    The number of treasures on a map must be over 0.
-}
data Level = Level Board.Board (Float, Float) Int Int deriving (Show)

{- nextLevel l
   PRE:       True
   POST:      A level with level number one above l.
   EXAMPLES:  nextLevel ==
-}
nextLevel :: Level -> Maybe (Level, (Float, Float))
nextLevel (Level _ _ n _) = setLevel (n+1)

{- setLevel i
   PRE:       If number of pre-defined levels
   EXAMPLES:  setLevel ==
-}
setLevel :: Int -> Maybe (Level, (Float, Float))
setLevel i
  | (length Resources.levels) - 1 < i = Nothing
  | otherwise =
    let
      tiles = Resources.levels !! i
      coord = Resources.coords !! i
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
   PRE:       True
   POST:      b
   EXAMPLES:  getBoard ==
-}
getBoard :: Level -> Board.Board
getBoard (Level b _ _ _) = b

{- getLevelGoal l
   PRE:       True
   POST:      The level goal of l.
   EXAMPLES:  getLevelGoal ==
-}
getLevelGoal :: Level -> Int
getLevelGoal (Level _ _ _ g) = g

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
         PRE:       ...
         POST:      post-condition on the result, in terms of the arguments
         EXAMPLES:  foundTreasure ==
      -}
      foundTreasure :: Board.Board -> (Int, Int) -> Bool
      foundTreasure board position = Tile.hasTreasure (board ! position)

{- spawnPosition l
   PRE:       True.
   POST:      Default spawn position for AIs.
   EXAMPLES:  spawnPosition ==
-}
spawnPosition :: Level -> (Float, Float)
spawnPosition (Level _ p _ _) = p
