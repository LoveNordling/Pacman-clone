module Tile(Actor(..), Tile(..)) where

data Actor = Void | Player | Computer deriving (Show, Eq)


-- Top, left, bottom, right
data Tile = Tile (Bool, Bool, Bool, Bool) Actor deriving (Show, Eq)
