module Core.Board.Board (Board, Tiles, createBoard, map1, map2) where
import Data.Array
import qualified Core.Board.Tile as Tile

-- The board/map
type Board = Array (Int, Int) Tile.Tile

-- The elements of a board
type Tiles = [Tile.Tile]

type Matrix = [Tiles]

-- elements of a map used for generating map
itemFloor = Tile.Floor (0,0) True
baseFloor = Tile.Floor (0, 0) False
baseWall  = Tile.Wall (0, 0)

{- createBoard m
   PRE:       Each row in m must have the same number of elements.
   POST:      An indexable list with elements from m, where an elements index is the position of the element in m.
   EXAMPLES:  generateBoard [ [baseWall, baseWall], [baseFloor, baseWall] ] == array ((0,0),(1,1)) [((0,0), Wall (0,0)), ((0,1), Wall (0,1)), ((1,0), Floor (1,0)), ((1,1), Wall (1,1))]
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
         POST:      acc with tiles based on elements in m.
         EXAMPLES:  generateBoard [ [baseWall, baseWall], [baseFloor, baseWall] ] 0 [] == [Wall (0,0), Wall (0,1), Floor (1,0), Wall (1,1)]
         VARIANT:   |m|
      -}
      generateBoard :: Matrix -> Int -> Tiles -> Tiles
      generateBoard []         _ acc = acc
      generateBoard (row:rows) x acc =
        generateBoard rows (x+1) (acc ++ (generateRow row 0))
        where
          {- generateRow b c
             PRE:       True
             POST:      Tiles based on b with position c.
             EXAMPLES:  generateRow [ baseWall, baseWall ] 0 0 == [ Wall (0,0), Wall (0,1) ]
                        generateRow [ baseWall, baseWall ] 1 0 == [ Wall (1,0), Wall (1,1) ]
                        generateRow [ ] 0 0                    == [ ]
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
  [baseWall, baseFloor, baseWall, baseFloor, baseFloor, baseWall, baseFloor, itemFloor, baseFloor, baseWall],
  [baseWall, baseFloor, baseWall, baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseFloor, baseWall],
  [baseWall, baseFloor, baseWall, baseFloor, baseFloor, baseWall, baseFloor, baseFloor, baseFloor, baseWall],
  [baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseWall, baseFloor, baseWall, baseFloor, baseWall],
  [baseWall, baseFloor, baseWall, baseFloor, baseFloor, baseWall, baseFloor, baseWall, baseFloor, baseWall],
  [baseWall, baseFloor, baseWall, baseWall, baseFloor, baseFloor, baseFloor, baseWall, baseFloor, baseWall],
  [baseWall, baseFloor, itemFloor, baseFloor, baseFloor, baseWall, baseFloor, itemFloor, baseFloor, baseWall],
  [baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall, baseWall]
  ]

--- Map 2 TODO: Needs update
map2 = createBoard hardcodedTiles2

hardcodedTiles2 = [
    [baseWall, baseWall, baseWall, baseWall],
    [baseWall, baseFloor, baseFloor, baseWall],
    [baseWall, baseFloor, baseFloor, baseWall],
    [baseWall, baseWall, baseWall, baseWall]
  ]
