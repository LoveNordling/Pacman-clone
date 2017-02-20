module Core.Board.Tile (Tile(..), setPosition) where

{-
  REPRESENTATION CONVENTION:
    A tile is an unmovable piece on the board.

    A floor is a valid path for a computer or the player and is given by Floor (a, b) t where (a, b) are the coordinates of the floor and t signifies if there is a treasure on that floor.

    A wall is an invalid path given by Wall (a, b), where (a,b) are the coordinates of the wall.

  REPRESENTATION INVARIANT:
    Tiles cannot have overlapping positions.
-}
data Tile = Floor (Int, Int) Treasure | Wall (Int, Int) deriving (Show, Eq)

type Treasure = Bool

{- position a
   PRE:       True
   POST:      The current position of a.
   EXAMPLES:  position (Floor (1, 1)) == (1, 1)
-}
position :: Tile -> (Int, Int)
position (Floor a _) = a
position (Wall a)  = a

{- setPosition t x y
   PRE:       True
   POST:      t with new position (x, y)
   EXAMPLES:  setPosition (Floor (0,0) False) 5 5 == Floor (5, 5) False
-}
setPosition :: Tile -> Int -> Int -> Tile
setPosition (Floor _ i) x y = Floor (x, y) i
setPosition (Wall _)    x y = Wall (x, y)
