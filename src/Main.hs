module Main where
import Graphic
import Graphics.Gloss.Interface.Pure.Game
import Tile

-- data Actor = Void | Player | Computer
-- -- Top, left, bottom, right
-- data Tile = Tile (Bool, Bool, Bool, Bool) Actor

-- roten ur antalet tiles måste vara ojämnt

-- tiles :: Board
-- tiles = Tile.generateBoard loveTiles
-- {-
-- -}
-- loveTiles = [
--     [Tile (True, True, True, False) Void, Tile (True, False, False, True) Void, Tile (True, True, False, True) Void],
--     [Tile (True, True, True, False) Void, Tile (False, False, False, True) Void, Tile (False, True, False, True) Void],
--     [Tile (True, True, False, False) Void, Tile (False, False, True, False) Void, Tile (False, False, True, True) Void],
--     [Tile (False, True, True, False) Void, Tile (True, False, True, False) Void, Tile (True, False, True, True) Void]
--     ]

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

latiles = [
  Tile (-2,2) (True, True, True, False) Void,
  Tile (-1,2) (True, False, False, True) Void,
  Tile (0, 2) (True,  False, False, True) Void,
  Tile (1, 2) (True, True, False, True) Void,
  Tile (2, 2) (True, True, False, True) Void,

  Tile (-2,1) (True, True, False, False) Void,
  Tile (-1,1) (False, False, False, True) Void,
  Tile (0, 1) (True, False, False, True) Void,
  Tile (1, 1) (True, True, False, True) Void,
  Tile (2, 1) (True, True, False, True) Void,

  Tile (-2,0) (False, True, True, False) Void,
  Tile (-1,0) (True, False, False, True) Void,
  Tile (0, 0) (True, False, False, True) Void,
  Tile (1, 0) (True, True, False, True) Void,
  Tile (2, 0) (True, True, False, True) Void,

  Tile (-2,-1) (True, True, True, False) Void,
  Tile (-1,-1) (True, False, False, True) Void,
  Tile (0, -1) (True, False, False, True) Void,
  Tile (1, -1) (True, True, False, True) Void,
  Tile (2, -1) (True, True, False, True) Void,

  Tile (-2,-2) (True, True, True, False) Void,
  Tile (-1,-2) (True, False, False, True) Void,
  Tile (0, -2) (True, False, False, True) Void,
  Tile (1, -2) (True, True, False, True) Void,
  Tile (2, -2) (True, True, False, True) Void
  ]

pacmantiles = [
  Tile (-6, 6) (True, True, False, True) Void,
  Tile (-5, 6) (True, False, True, False) Void,
  Tile (-4, 6) (True, False, True, False) Void,
  Tile (-3, 6) (True, False, True, False) Void,
  Tile (-2, 6) (True, False, False, True) Void,
  Tile (-1, 6) (True, True, True, True) Void,
  Tile (0, 6) (True, True, False, False) Void,
  Tile (1, 6) (True, False, True, False) Void,
  Tile (2, 6) (True, False, True, False) Void,
  Tile (3, 6) (True, False, True, False) Void,
  Tile (4, 6) (True, False, True, False) Void,
  Tile (5, 6) (True, False, True, False) Void,
  Tile (6, 6) (True, False, False, True) Void,

  Tile (-6, 5) (False, True, False, True) Void,
  Tile (-5, 5) (True, True, True, True) Void,
  Tile (-4, 5) (True, True, True, True) Void,
  Tile (-3, 5) (True, True, True, True) Void,
  Tile (-2, 5) (False, True, False, True) Void,
  Tile (-1, 5) (True, True, True, True) Void,
  Tile (0, 5) (False, True, False, True) Void,
  Tile (1, 5) (True, True, True, True) Void,
  Tile (2, 5) (True, True, True, True) Void,
  Tile (3, 5) (True, True, True, True) Void,
  Tile (4, 5) (True, True, True, True) Void,
  Tile (5, 5) (True, True, True, True) Void,
  Tile (6, 5) (False, True, False, False) Void,

  Tile (-6, 4) (False, True, False, True) Void,
  Tile (-5, 4) (True, True, True, True) Void,
  Tile (-4, 4) (True, True, True, False) Void,
  Tile (-3, 4) (True, False, True, False) Void,
  Tile (-2, 4) (False, False, False, True) Void,
  Tile (-1, 4) (True, True, True, True) Void,
  Tile (0, 4) (False, True, False, False) Void,
  Tile (1, 4) (True, False, False, False) Void,
  Tile (2, 4) (True, False, False, False) Void,
  Tile (3, 4) (True, False, True, False) Void,
  Tile (4, 4) (True, False, True, False) Void,
  Tile (5, 4) (True, False, True, False) Void,
  Tile (6, 4) (False, False, False, True) Void,

  Tile (-6, 3) (False, True, False, True) Void,
  Tile (-5, 3) (True, True, True, True) Void,
  Tile (-4, 3) (True, True, True, True) Void,
  Tile (-3, 3) (True, True, True, True) Void,
  Tile (-2, 3) (False, True, False, False) Void,
  Tile (-1, 3) (True, False, True, False) Void,
  Tile (0, 3) (False, False, False, False) Void,
  Tile (1, 3) (False, False, False, False) Void,
  Tile (2, 3) (False, False, False, True) Void,
  Tile (3, 3) (True, True, True, True) Void,
  Tile (4, 3) (True, True, True, True) Void,
  Tile (5, 3) (True, True, True, True) Void,
  Tile (6, 3) (False, True, False, True) Void,

  Tile (-6, 2) (False, True, False, False) Void,
  Tile (-5, 2) (True, False, True, False) Void,
  Tile (-4, 2) (True, False, True, False) Void,
  Tile (-3, 2) (True, False, True, False) Void,
  Tile (-2, 2) (False, False, False, True) Void,
  Tile (-1, 2) (True, True, True, True) Void,
  Tile (0, 2) (False, True, False, False) Void,
  Tile (1, 2) (False, False, False, False) Void,
  Tile (2, 2) (False, False, False, True) Void,
  Tile (3, 2) (True, True, True, True) Void,
  Tile (4, 2) (True, True, False, False) Void,
  Tile (5, 2) (True, False, False, False) Void,
  Tile (6, 2) (False, False, False, True) Void,

  Tile (-6, 1) (False, True, False, True) Void,
  Tile (-5, 1) (True, True, True, True) Void,
  Tile (-4, 1) (True, True, True, True) Void,
  Tile (-3, 1) (True, True, True, True) Void,
  Tile (-2, 1) (False, True, False, False) Void,
  Tile (-1, 1) (True, False, True, False) Void,
  Tile (0, 1) (False, False, True, False) Void,
  Tile (1, 1) (False, False, True, False) Void,
  Tile (2, 1) (False, False, True, True) Void,
  Tile (3, 1) (True, True, True, True) Void,
  Tile (4, 1) (False, True, True, False) Void,
  Tile (5, 1) (False, False, False, False) Void,
  Tile (6, 1) (False, False, False, True) Void,

  Tile (-6, 0) (False, True, True, False) Void,
  Tile (-5, 0) (True, False, True, False) Void,
  Tile (-4, 0) (True, False, True, False) Void,
  Tile (-3, 0) (True, False, True, False) Void,
  Tile (-2, 0) (False, False, False, True) Void,
  Tile (-1, 0) (True, True, True, True) Void,
  Tile (0, 0) (True, True, True, True) Void,
  Tile (1, 0) (True, True, True, True) Void,
  Tile (2, 0) (True, True, True, True) Void,
  Tile (3, 0) (True, True, True, True) Void,
  Tile (4, 0) (True, True, True, False) Void,
  Tile (5, 0) (False, True, False, False) Void,
  Tile (6, 0) (False, False, False, True) Void,

  Tile (-6, -1) (True, True, True, True) Void,
  Tile (-5, -1) (True, True, True, True) Void,
  Tile (-4, -1) (True, True, True, True) Void,
  Tile (-3, -1) (True, True, True, True) Void,
  Tile (-2, -1) (False, True, False, False) Void,
  Tile (-1, -1) (True, False, True, False) Void,
  Tile (0, -1) (True, False, True, False) Void,
  Tile (1, -1) (True, False, True, False) Void,
  Tile (2, -1) (True, False, True, False) Void,
  Tile (3, -1) (True, False, True, False) Void,
  Tile (4, -1) (True, False, True, False) Void,
  Tile (5, -1) (True, False, True, False) Void,
  Tile (6, -1) (False, False, False, True) Void,

  Tile (-6, -2) (True, True, False, True) Void,
  Tile (-5, -2) (True, True, True, True) Void,
  Tile (-4, -2) (True, True, True, False) Void,
  Tile (-3, -2) (True, False, True, False) Void,
  Tile (-2, -2) (False, False, False, True) Void,
  Tile (-1, -2) (True, True, True, True) Void,
  Tile (0, -2) (True, False, True, False) Void,
  Tile (1, -2) (True, True, True, True) Void,
  Tile (2, -2) (True, True, True, True) Void,
  Tile (3, -2) (True, True, True, True) Void,
  Tile (4, -2) (True, True, True, True) Void,
  Tile (5, -2) (True, True, True, True) Void,
  Tile (6, -2) (False, True, False, True) Void,

  Tile (-6, -3) (False, True, False, True) Void,
  Tile (-5, -3) (True, True, True, True) Void,
  Tile (-4, -3) (True, True, True, True) Void,
  Tile (-3, -3) (True, True, True, True) Void,
  Tile (-2, -3) (False, True, False, False) Void,
  Tile (-1, -3) (True, False, False, False) Void,
  Tile (0, -3) (True, False, True, False) Void,
  Tile (1, -3) (True, False, True, False) Void,
  Tile (2, -3) (True, False, False, False) Void,
  Tile (3, -3) (True, False, False, True) Void,
  Tile (4, -3) (True, True, True, True) Void,
  Tile (5, -3) (False, True, False, False) Void,
  Tile (6, -3) (False, False, False, True) Void,

  Tile (-6, -4) (False, True, False, False) Void,
  Tile (-5, -4) (True, False, False, True) Void,
  Tile (-4, -4) (True, True, True, True) Void,
  Tile (-3, -4) (True, True, False, False) Void,
  Tile (-2, -4) (False, False, False, False) Void,
  Tile (-1, -4) (False, False, False, True) Void,
  Tile (0, -4) (True, True, True, True) Void,
  Tile (1, -4) (True, True, True, True) Void,
  Tile (2, -4) (False, True, True, False) Void,
  Tile (3, -4) (False, False, True, True) Void,
  Tile (4, -4) (True, True, True, True) Void,
  Tile (5, -4) (False, True, False, False) Void,
  Tile (6, -4) (False, False, False, True) Void,

  Tile (-6, -5) (False, True, False, False) Void,
  Tile (-5, -5) (False, False, False, True) Void,
  Tile (-4, -5) (True, True, True, True) Void,
  Tile (-3, -5) (False, True, False, False) Void,
  Tile (-2, -5) (False, False, False, False) Void,
  Tile (-1, -5) (False, False, False, False) Void,
  Tile (0, -5) (True, False, False, True) Void,
  Tile (1, -5) (True, True, True, True) Void,
  Tile (2, -5) (True, True, True, True) Void,
  Tile (3, -5) (True, True, True, True) Void,
  Tile (4, -5) (True, True, True, True) Void,
  Tile (5, -5) (False, True, False, False) Void,
  Tile (6, -5) (False, False, False, True) Void,

  Tile (-6, -6) (False, True, True, False) Void,
  Tile (-5, -6) (False, False, True, False) Void,
  Tile (-4, -6) (True, False, True, False) Void,
  Tile (-3, -6) (False, False, True, False) Void,
  Tile (-2, -6) (False, False, True, False) Void,
  Tile (-1, -6) (False, False, True, False) Void,
  Tile (0, -6) (True, False, True, True) Void,
  Tile (1, -6) (True, False, True, False) Void,
  Tile (2, -6) (True, False, True, False) Void,
  Tile (3, -6) (True, False, True, False) Void,
  Tile (4, -6) (True, False, True, False) Void,
  Tile (5, -6) (False, False, True, False) Void,
  Tile (6, -6) (False, False, True, True) Void
  ]

main :: IO ()
main = Graphic.renderMap pacmantiles
