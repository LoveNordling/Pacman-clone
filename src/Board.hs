module Board(Edges, Direction(..), Actor(..), Tile(..)) where
-- Top, left, bottom, right
type Edges = (Bool, Bool, Bool, Bool)
data Direction = North | South | East | West
data Actor = Void | Player Direction | Computer

{-
  REPRESENTATION CONVENTION:
    Tile e a gives a tile with edges e and actor a.
    How are the edges defined??
  REPRESENTATION INVARIANT:
    A tile can not have edges around the entire tile if the surrounding tiles
    have edges around them.
-}
data Tile = Tile Edges Actor
