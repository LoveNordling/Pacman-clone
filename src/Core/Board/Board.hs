module Core.Board.Board (Board, Tiles, createBoard, map1, map2) where
import Data.Array
import qualified Core.Board.Tile as Tile

type Board = Array (Int, Int) Tile.Tile
type Tiles = [Tile.Tile]
type Matrix = [Tiles]

baseFloor = Tile.Floor (0, 0)
baseWall  = Tile.Wall (0, 0)

{- createBoard m
   PRE:       Each row in m must have the same number of elements.
   POST:      An array based on m where index is the position of the elements in m.
   EXAMPLES:  generateBoard  ==
-}
createBoard :: Matrix -> Board
createBoard board =
  let
    rows = length (board) - 1
    cols = length (board !! 0) - 1
  in
    listArray ( (0,0), (rows, cols) ) (generateBoard board 0 [])
    where
      {- generateBoard m r acc
         PRE:       Each row in m must have the same number of elements.
         POST:      A board with the pieces' position based on the position of them in m.
         EXAMPLES:  generateBoard  ==
         VARIANT:   |m|
      -}
      generateBoard :: Matrix -> Int -> Tiles -> Tiles
      generateBoard []         _ acc = acc
      generateBoard (row:rows) x acc =
        generateBoard rows (x+1) (acc ++ (generateRow row 0))
        where
          {- generateRow b c
             PRE:       True
             POST:      A board with pieces with
             EXAMPLES:  generateRow ==
             VARIANT:   |b|
          -}
          generateRow :: Tiles -> Int -> Tiles
          generateRow []     _ = []
          generateRow (t:ts) y = (Tile.setPosition t x y):(generateRow ts (y+1))


---- MAP 1
map1 = createBoard hardcodedTiles1

hardcodedTiles1 = [
  [baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall],
  [baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseWall, baseFloor, baseWall, baseFloor, baseWall],
  [baseWall, baseFloor, baseWall, baseFloor, baseFloor, baseWall, baseFloor, baseFloor, baseFloor, baseWall],
  [baseWall, baseFloor, baseWall, baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseFloor, baseWall],
  [baseWall, baseFloor, baseWall, baseFloor, baseFloor, baseWall, baseFloor, baseFloor, baseFloor, baseWall],
  [baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseWall, baseFloor, baseWall, baseFloor, baseWall],
  [baseWall, baseFloor, baseWall, baseFloor, baseFloor, baseWall, baseFloor, baseWall, baseFloor, baseWall],
  [baseWall, baseFloor, baseWall, baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseFloor, baseWall],
  [baseWall, baseFloor, baseFloor, baseFloor, baseFloor, baseWall, baseFloor, baseFloor, baseFloor, baseWall],
  [baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall]
  ]

map2 = createBoard hardcodedTiles2

hardcodedTiles2 = [
    [baseWall, baseWall, baseWall, baseWall],
    [baseWall, baseFloor, baseFloor, baseWall],
    [baseWall, baseFloor, baseFloor, baseWall],
    [baseWall, baseWall, baseWall, baseWall]
  ]
