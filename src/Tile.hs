module Tile(Actor(..), Tile(..), generateBoard, Position, Board) where

-- Suppress HLint messages
{-# ANN module "HLint: Ignore Use mappend" #-}

data Actor = Void | Player | Computer deriving (Show, Eq)
-- Top, left, bottom, right
data Tile = Tile (Bool, Bool, Bool, Bool) Actor deriving (Show, Eq)

type Coords    = (Int,Int)
type Position  = (Float, Float)
type Board     = [(Position, Tile)]

{-
	generateBoard matrix
	PRE:
	POST: a list of tupels of a tile as snd arg and it's position as its first
-}

generateBoard :: [[Tile]] -> Board
generateBoard t@(x:xs) = generateBoardAux t (-(length x)//2) ((length t)//2)

generateBoardAux :: [[Tile]] -> Float -> Float -> Board
generateBoardAux [] _ _ = []
generateBoardAux (t:ts) x y = (generateBoardRow t x y) ++ generateBoardAux ts x (y-1)

generateBoardRow :: [Tile] -> Float -> Float -> Board
generateBoardRow [] _ _ = []
generateBoardRow (t:ts) x y = ((x,y),t) : (generateBoardRow ts (x+1) y)


(//) :: Int -> Int -> Float
(//) a b = fromIntegral (div a b)
