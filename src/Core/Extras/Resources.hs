module Core.Extras.Resources
        ( dazzleManRight, dazzleManLeft, monsterRight, monsterLeft
        , levels, coords )
where

import qualified Core.Board.Tile as Tile

-- Path to the images used for the characters
dazzleManRight, dazzleManLeft, monsterLeft, monsterRight :: String
dazzleManRight = "res/dazzleManRight.png"
dazzleManLeft  = "res/dazzleManLeft.png"
monsterLeft    = "res/monsterLeft.png"
monsterRight   = "res/monsterRight.png"

-- TODO: NEED TO FIGURE OUT A BETTER WAY FOR ALL OF THIS SHIT
-------------------------------------------
-- LEVELS
-------------------------------------------
u = Tile.Floor (0,0) True
o = Tile.Floor (0, 0) False
x = Tile.Wall (0, 0)

-- first element of the tuple is player start coord, second is AI start coord
coords :: [ ((Float, Float), (Float, Float)) ]
coords = [ ((1, 1), (8, 8)), ((1, 1), (10, 11)) ]
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
   [x, x, x, x, x, x, x, x, x, x],
   [x, o, o, o, x, x, o, x, o, x],
   [x, o, x, o, o, x, o, u, o, x],
   [x, o, x, x, o, o, o, x, o, x],
   [x, o, x, o, o, x, o, o, o, x],
   [x, o, o, o, x, x, o, x, o, x],
   [x, o, x, o, o, x, o, x, o, x],
   [x, o, x, x, o, o, o, x, o, x],
   [x, o, u, o, o, x, o, u, o, x],
   [x, x, x, x, x, x, x, x, x, x]
 ]
