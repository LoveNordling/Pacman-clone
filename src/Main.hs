module Main where
import Graphic
import Tile

-- data Actor = Void | Player | Computer
-- -- Top, left, bottom, right
-- data Tile = Tile (Bool, Bool, Bool, Bool) Actor

-- roten ur antalet tiles måste vara ojämnt
tiles :: Board
tiles = [
  ((-1, 1), Tile (True, True, True, False) Void) ,
  ((0, 1), Tile (True, False, False, True) Void) ,
  ((1, 1), Tile (True, True, False, True) Void) ,

  ((-1, 0), Tile (True, True, False, True) Void),
  ((0, 0), Tile (False, False, False, False) Void),
  ((1, 0), Tile (False, True, False, True) Void),

  ((-1, -1), Tile (False, True, True, False) Void),
  ((0, -1), Tile (False, False, True, False) Void),
  ((1, -1), Tile (False, False, True, True) Void)

  ]

--
-- tiles = [
--   ((-2, 2), Tile (True, True, True, False) Void) ,
--   ((-1, 2), Tile (True, False, False, True) Void) ,
--   ((0, 2), Tile (True, False, False, True) Void) ,
--   ((1, 2), Tile (True, True, False, True) Void) ,
--   ((2, 2), Tile (True, True, False, True) Void) ,
--
--   ((-2, 1), Tile (True, True, True, False) Void) ,
--   ((-1, 1), Tile (True, False, False, True) Void) ,
--   ((0, 1), Tile (True, False, False, True) Void) ,
--   ((1, 1), Tile (True, True, False, True) Void) ,
--   ((2, 1), Tile (True, True, False, True) Void) ,
--
--   ((-2, 0), Tile (True, True, True, False) Void) ,
--   ((-1, 0), Tile (True, False, False, True) Void) ,
--   ((0, 0), Tile (True, False, False, True) Void) ,
--   ((1, 0), Tile (True, True, False, True) Void) ,
--   ((2, 0), Tile (True, True, False, True) Void) ,
--
--   ((-2, -1), Tile (True, True, True, False) Void) ,
--   ((-1, -1), Tile (True, False, False, True) Void) ,
--   ((0, -1), Tile (True, False, False, True) Void) ,
--   ((1, -1), Tile (True, True, False, True) Void) ,
--   ((2, -1), Tile (True, True, False, True) Void) ,
--
--   ((-2, -2), Tile (True, True, True, False) Void) ,
--   ((-1, -2), Tile (True, False, False, True) Void) ,
--   ((0, -2), Tile (True, False, False, True) Void) ,
--   ((1, -2), Tile (True, True, False, True) Void) ,
--   ((2, -2), Tile (True, True, False, True) Void)
--   ]

main :: IO ()
main = Graphic.renderMap tiles
