<<<<<<< HEAD
module Tile(Board, GameState(..), standardTiles) where

import Core.Board.Actor
import Core.Board.Tile
=======
module Tile(Actor(..), Type(..), Tile(..), generateBoard, Position, Board) where
>>>>>>> 83f1c0863a65425d813fbac41b6a2db22b102700

-- Suppress HLint messages
{-# ANN module "HLint: Ignore Use mappend" #-}

<<<<<<< HEAD
-- TODO: Write representation convention

data GameState = State [Tile] Actor Actor
type Board = [Tile]

baseFloor = Floor (0, 0) []
baseWall = Wall (0, 0)

{-
	generateBoard x
	PURPOSE: to generate a board with tile coordinates from x
	Pre: True
	Post: A Board based on the list of list of tiles
-}
generateBoard :: [Board] -> Board
generateBoard t@(x:xs) = generateBoardAux t (-(fromIntegral (length x))/2) ((fromIntegral (length t))/2)
=======

data Tile = Floor (Float, Float) | Wall (Float, Float) deriving (Show)

type Position  = (Float, Float)
type Board     = [Tile]
type Path = [Position]

{-
	generateBoard matrix
	PRE:
	POST: a list of tupels of a tile as snd arg and it's position as its first
-}

generateBoard :: [[Tile]] -> Board
generateBoard t@(x:xs) = generateBoardAux t (-(length x)//2) ((length t)//2)
>>>>>>> 83f1c0863a65425d813fbac41b6a2db22b102700

generateBoardAux :: [[Tile]] -> Float -> Float -> Board
generateBoardAux [] _ _ = []
generateBoardAux (t:ts) x y = (generateBoardRow t x y) ++ generateBoardAux ts x (y-1)

generateBoardRow :: [Tile] -> Float -> Float -> Board
generateBoardRow [] _ _ = []
<<<<<<< HEAD
generateBoardRow ((Floor (_, _) _):ts) x y = (Floor (x, y) []) : (generateBoardRow ts (x+1) y)
generateBoardRow ((Wall (_, _)):ts) x y = (Wall (x, y)) : (generateBoardRow ts (x+1) y)


standardTiles = generateBoard hardCodedTiles

hardCodedTiles =
	[
	[baseWall , baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseFloor, baseWall],
	[baseWall , baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall]
	]

{-
ArdiTiles =
[
  Floor (-6, 6) [],
  Floor (-5, 6) [],
  Floor (-4, 6) [],
  Floor (-3, 6) [],
  Floor (-2, 6) [],
  Wall (-1, 6),
  Floor (0, 6) [],
  Floor (1, 6) [],
  Floor (2, 6) [],
  Floor (3, 6) [],
  Floor (4, 6) [],
  Floor (5, 6) [],
  Floor (6, 6) [],

  Floor (-6, 5) [],
  Wall (-5, 5),
  Wall (-4, 5),
  Wall (-3, 5),
  Floor (-2, 5) [],
  Wall (-1, 5),
  Floor (0, 5) [],
  Wall (1, 5),
  Wall (2, 5),
  Wall (3, 5),
  Wall (4, 5),
  Wall (5, 5),
  Floor (6, 5) [],

  Floor (-6, 4) [],
  Wall (-5, 4),
  Floor (-4, 4) [],
  Floor (-3, 4) [],
  Floor (-2, 4) [],
  Wall (-1, 4),
  Floor (0, 4) [],
  Floor (1, 4) [],
  Floor (2, 4) [],
  Floor (3, 4) [],
  Floor (4, 4) [],
  Floor (5, 4) [],
  Floor (6, 4) [],

  Floor (-6, 3) [],
  Wall (-5, 3),
  Wall (-4, 3),
  Wall (-3, 3),
  Floor (-2, 3) [],
  Floor (-1, 3) [],
  Floor (0, 3) [],
  Floor (1, 3) [],
  Floor (2, 3) [],
  Wall (3, 3),
  Wall (4, 3),
  Wall (5, 3),
  Floor (6, 3) [],

  Floor (-6, 2) [],
  Floor (-5, 2) [],
  Floor (-4, 2) [],
  Floor (-3, 2) [],
  Floor (-2, 2) [],
  Wall (-1, 2),
  Floor (0, 2) [],
  Floor (1, 2) [],
  Floor (2, 2) [],
  Wall (3, 2),
  Floor (4, 2) [],
  Floor (5, 2) [],
  Floor (6, 2) [],

  Floor (-6, 1) [],
  Wall (-5, 1),
  Wall (-4, 1),
  Wall (-3, 1),
  Floor (-2, 1) [],
  Floor (-1, 1) [],
  Floor (0, 1) [],
  Floor (1, 1) [],
  Floor (2, 1) [],
  Wall (3, 1),
  Floor (4, 1) [],
  Floor (5, 1) [],
  Floor (6, 1) [],

  Floor (-6, 0) [],
  Floor (-5, 0) [],
  Floor (-4, 0) [],
  Floor (-3, 0) [],
  Floor (-2, 0) [],
  Wall (-1, 0),
  Wall (0, 0),
  Wall (1, 0),
  Wall (2, 0),
  Wall (3, 0),
  Wall (4, 0),
  Wall (5, 0),
  Floor (6, 0) [],

  Wall (-6, -1),
  Wall (-5, -1),
  Wall (-4, -1),
  Wall (-3, -1),
  Floor (-2, -1) [],
  Floor (-1, -1) [],
  Floor (0, -1) [],
  Floor (1, -1) [],
  Floor (2, -1) [],
  Floor (3, -1) [],
  Floor (4, -1) [],
  Floor (5, -1) [],
  Floor (6, -1) [],

  Floor (-6, -2) [],
  Wall (-5, -2),
  Floor (-4, -2) [],
  Floor (-3, -2) [],
  Floor (-2, -2) [],
  Wall (-1, -2),
  Wall (0, -2),
  Wall (1, -2),
  Wall (2, -2),
  Wall (3, -2),
  Wall (4, -2),
  Wall (5, -2),
  Floor (6, -2) [],

  Floor (-6, -3) [],
  Wall (-5, -3),
  Wall (-4, -3),
  Wall (-3, -3),
  Floor (-2, -3) [],
  Floor (-1, -3) [],
  Floor (0, -3) [],
  Floor (1, -3) [],
  Floor (2, -3) [],
  Floor (3, -3) [],
  Floor (4, -3) [],
  Wall (5, -3),
  Floor (6, -3) [],

  Floor (-6, -4) [],
  Floor (-5, -4) [],
  Wall (-4, -4),
  Floor (-3, -4) [],
  Floor (-2, -4) [],
  Floor (-1, -4) [],
  Wall (0, -4),
  Wall (1, -4),
  Floor (2, -4) [],
  Floor (3, -4) [],
  Floor (4, -4) [],
  Wall (5, -4),
  Floor (6, -4) [],

  Floor (-6, -5) [],
  Floor (-5, -5) [],
  Wall (-4, -5),
  Floor (-3, -5) [],
  Floor (-2, -5) [],
  Floor (-1, -5) [],
  Floor (0, -5) [],
  Wall (1, -5),
  Wall (2, -5),
  Wall (3, -5),
  Wall (4, -5),
  Wall (5, -5),
  Floor (6, -5) [],

  Floor (-6, -6) [],
  Floor (-5, -6) [],
  Floor (-4, -6) [],
  Floor (-3, -6) [],
  Floor (-2, -6) [],
  Floor (-1, -6) [],
  Floor (0, -6) [],
  Floor (1, -6) [],
  Floor (2, -6) [],
  Floor (3, -6) [],
  Floor (4, -6) [],
  Floor (5, -6) [],
  Floor (6, -6) []
  ]
-}
=======
generateBoardRow (t:ts) x y = ((x,y),t) : (generateBoardRow ts (x+1) y)


(//) :: Int -> Int -> Float
(//) a b = fromIntegral (div a b)
>>>>>>> 83f1c0863a65425d813fbac41b6a2db22b102700
