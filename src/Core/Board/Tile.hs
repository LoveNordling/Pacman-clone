module Core.Board.Tile (Tile(..), setPosition) where

{-
  REPRESENTATION CONVENTION:
    A tile is an unmovable piece on the board.

    A floor is a valid path for a computer or the player and is given by Floor (a, b) where (a, b) are the coordinates of the floor. A wall is an invalid path given by Wall (a, b), where (a,b) are the coordinates of the wall.
  REPRESENTATION INVARIANT:
    Tiles cannot have overlapping positions.

-}
data Tile = Floor (Int, Int) | Wall (Int, Int) deriving (Show)

{- position a
   PRE:       True
   POST:      The current position of a.
   EXAMPLES:  position (Floor (1, 1)) == (1, 1)
-}
position :: Tile -> (Int, Int)
position (Floor a) = a
position (Wall a)  = a

setPosition :: Tile -> Int -> Int -> Tile
setPosition (Floor _) x y = Floor (x, y)
setPosition (Wall _)  x y = Wall (x, y)
